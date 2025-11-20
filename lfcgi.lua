--[[
  lua-fastcgi
  - lua 5.1, 5.2, 5.3+, and LuaJIT.
  - requires luasocket
  - soft requires bitop for 5.1 and 5.2

  MIT license
]]

local socket = require("socket")
local coroutine = coroutine

local M = {}

--------------------------------------------------------------------------------
-- BITWISE OP LOADER
--------------------------------------------------------------------------------
local bit = {}
do
  -- 1. check for lua 5.3+ builtin
  if _VERSION and tonumber(_VERSION:match("5%.(%d+)")) >= 3 then
    bit.band = function(a, b) return a & b end
    bit.bor = function(a, b) return a | b end
    bit.bxor = function(a, b) return a ~ b end
    bit.rshift = function(a, n) return a >> n end
    bit.lshift = function(a, n) return a << n end
    bit.bnot = function(a) return ~a end
  else
    -- 2. check for luajit builtin
    local has_jit, jit = pcall(require, "jit")
    if has_jit and jit.version then
        pcall(function() bit = require("bit") end)
    end

    -- 3. check for bitop lib
    if not bit.band then
        local has_bitop, bitop = pcall(require, "bitop")
        if has_bitop then bit = bitop end
    end

    -- 4. pure lua fallback (slow)
    if not bit.band then
        print("Warning: Using slow pure-Lua fallback for bitwise operations.")
        local function _band(a, b)
            local res, p = 0, 1
            while a > 0 and b > 0 do
                if a % 2 == 1 and b % 2 == 1 then res = res + p end
                a, b, p = math.floor(a/2), math.floor(b/2), p*2
            end
            return res
        end
        local function _bor(a, b)
            local res, p = 0, 1
            while a > 0 or b > 0 do
                if a % 2 == 1 or b % 2 == 1 then res = res + p end
                a, b, p = math.floor(a/2), math.floor(b/2), p*2
            end
            return res
        end
        bit.band = _band
        bit.bor = _bor
        bit.rshift = function(a, n) return math.floor(a / (2^n)) end
        bit.lshift = function(a, n) return a * (2^n) end
    end
  end

  if not bit.band then
    error("Could not find a suitable bitwise operation library and fallback failed.")
  end
end


--------------------------------------------------------------------------------
-- CONSTANTS
--------------------------------------------------------------------------------
M.constants = {
    FCGI_VERSION_1         = 1,
    -- record types
    FCGI_BEGIN_REQUEST     = 1,
    FCGI_ABORT_REQUEST     = 2,
    FCGI_END_REQUEST       = 3,
    FCGI_PARAMS            = 4,
    FCGI_STDIN             = 5,
    FCGI_STDOUT            = 6,
    FCGI_STDERR            = 7,
    FCGI_DATA              = 8,
    FCGI_GET_VALUES        = 9,
    FCGI_GET_VALUES_RESULT = 10,
    FCGI_UNKNOWN_TYPE      = 11,
    -- roles
    FCGI_RESPONDER         = 1,
    FCGI_AUTHORIZER        = 2,
    FCGI_FILTER            = 3,
    -- protocol status
    FCGI_REQUEST_COMPLETE  = 0,
    FCGI_CANT_MPX_CONN     = 1,
    FCGI_OVERLOADED        = 2,
    FCGI_UNKNOWN_ROLE      = 3,
    -- flags
    FCGI_KEEP_CONN         = 1,
    -- limits and sizes
    FCGI_HEADER_LEN        = 8,
    FCGI_MAX_CONTENT_LEN   = 65535,
}
local C = M.constants

--------------------------------------------------------------------------------
-- BINARY PACKING HELPERS
--------------------------------------------------------------------------------
local function pack_u16_be(n)
    return string.char(bit.rshift(n, 8), n % 256)
end

local function unpack_u16_be(s, pos)
    pos = pos or 1
    local b1, b2 = string.byte(s, pos, pos + 1)
    return b1 * 256 + b2
end

local function unpack_vlen(s, pos)
    local b1 = string.byte(s, pos)
    if bit.band(b1, 0x80) == 0 then
        return b1, pos + 1
    else
        local b2, b3, b4 = string.byte(s, pos + 1, pos + 3)
        local len = bit.bor(bit.lshift(bit.band(b1, 0x7F), 24), bit.lshift(b2, 16), bit.lshift(b3, 8), b4)
        return len, pos + 4
    end
end

local function pack_vlen(n)
    if n < 128 then
        return string.char(n)
    else
        return string.char(
            bit.bor(bit.rshift(n, 24), 0x80),
            bit.band(bit.rshift(n, 16), 0xFF),
            bit.band(bit.rshift(n, 8), 0xFF),
            bit.band(n, 0xFF)
        )
    end
end

local function unpack_params(data)
    local params = {}
    local pos = 1
    while pos <= #data do
        local name_len, next_pos = unpack_vlen(data, pos)
        pos = next_pos
        local val_len, next_pos = unpack_vlen(data, pos)
        pos = next_pos
        local name = string.sub(data, pos, pos + name_len - 1)
        pos = pos + name_len
        local value = string.sub(data, pos, pos + val_len - 1)
        pos = pos + val_len
        params[name] = value
    end
    return params
end

local function pack_name_values(tbl)
    local parts = {}
    for name, value in pairs(tbl) do
        table.insert(parts, pack_vlen(#name))
        table.insert(parts, pack_vlen(#value))
        table.insert(parts, name)
        table.insert(parts, value)
    end
    return table.concat(parts)
end

--------------------------------------------------------------------------------
-- RECORD HANDLING
--------------------------------------------------------------------------------

local function pack_header(rec_type, req_id, content_len, padding_len)
    return string.char(
        C.FCGI_VERSION_1, rec_type,
        bit.rshift(req_id, 8), req_id % 256,
        bit.rshift(content_len, 8), content_len % 256,
        padding_len, 0
    )
end

local function unpack_header(data)
    local b = { string.byte(data, 1, C.FCGI_HEADER_LEN) }
    return {
        version = b[1], type = b[2],
        requestId = b[3] * 256 + b[4],
        contentLength = b[5] * 256 + b[6],
        paddingLength = b[7],
    }
end

-- read n bytes from socket
local function recv_exact(sock, n)
	if n == 0 then return "" end
	local chunks, got = {}, 0
	while got < n do
		local chunk, err, partial = sock:receive(n - got)
		if not chunk then
			if partial and #partial > 0 then table.insert(chunks, partial) end
			return nil, err
		end
		table.insert(chunks, chunk)
		got = got + #chunk
	end
	return table.concat(chunks)
end

local function send_record(conn, rec_type, req_id, content)
    content = content or ""
    local content_len = #content
    local padding_len = (8 - (content_len % 8)) % 8
    local padding = string.rep("\0", padding_len)
    local header = pack_header(rec_type, req_id, content_len, padding_len)
    return conn:send(header .. content .. padding)
end

-- stream writer for STDOUT/STDERR
local function new_stream_writer(conn, rec_type, req_id)
    return {
        write = function(_, data)
            if not data or #data == 0 then return true end
            local pos = 1
            while pos <= #data do
                local chunk_size = math.min(C.FCGI_MAX_CONTENT_LEN, #data - pos + 1)
                local chunk = string.sub(data, pos, pos + chunk_size - 1)
                local ok, err = send_record(conn, rec_type, req_id, chunk)
                if not ok then return false, err end
                pos = pos + chunk_size
            end
            return true
        end
    }
end

--------------------------------------------------------------------------------
-- SERVER
--------------------------------------------------------------------------------

local Request = {}
Request.__index = Request

function Request:new(id, conn, keep_conn)
    local o = setmetatable({}, self)
    o.id = id; o.conn = conn; o.keep_conn = keep_conn
    o.params = {}; o.params_buffer = ""
    o.stdin_buffer = ""; o.stdin_closed = false
    o.stdin_reader_co = nil; o.responded = false
    o.stdout = new_stream_writer(conn, C.FCGI_STDOUT, id)
    o.stderr = new_stream_writer(conn, C.FCGI_STDERR, id)
    return o
end

function Request:add_params(data)
    if #data > 0 then
        self.params_buffer = self.params_buffer .. data
    else
        local p = unpack_params(self.params_buffer)
        for k,v in pairs(p) do self.params[k] = v end
        self.params_buffer = nil -- free memory
    end
end

function Request:add_stdin(data)
    if #data > 0 then
        self.stdin_buffer = self.stdin_buffer .. data
    else
        self.stdin_closed = true
    end
    if self.stdin_reader_co and coroutine.status(self.stdin_reader_co) == "suspended" then
        coroutine.resume(self.stdin_reader_co)
    end
end

function Request:get_stdin_reader()
    local self = self
    return {
        read = function(_, count)
            if count == 0 then return "" end
            if count then
                while #self.stdin_buffer < count and not self.stdin_closed do
                    self.stdin_reader_co = coroutine.running()
                    coroutine.yield()
                end
                local chunk = string.sub(self.stdin_buffer, 1, count)
                self.stdin_buffer = string.sub(self.stdin_buffer, #chunk + 1)
                return chunk
            else
                while not self.stdin_closed do
                    self.stdin_reader_co = coroutine.running()
                    coroutine.yield()
                end
                local data = self.stdin_buffer
                self.stdin_buffer = ""
                return data
            end
        end
    }
end

function Request:finish(app_status, proto_status)
    if self.responded then return end
    self.responded = true
    app_status = app_status or 0
    proto_status = proto_status or C.FCGI_REQUEST_COMPLETE
    send_record(self.conn, C.FCGI_STDOUT, self.id, "")
    send_record(self.conn, C.FCGI_STDERR, self.id, "")
    local status_bytes = {
        bit.rshift(app_status, 24) % 256, bit.rshift(app_status, 16) % 256,
        bit.rshift(app_status, 8) % 256, app_status % 256
    }
    local body = string.char(table.unpack(status_bytes)) .. string.char(proto_status, 0, 0, 0)
    send_record(self.conn, C.FCGI_END_REQUEST, self.id, body)
end

-- main server loop
local function handle_connection(conn, handler, opts)
    local requests = {}
    local active = true
    while active do
        local header_data, err = recv_exact(conn, C.FCGI_HEADER_LEN)
        if not header_data then break end
        local header = unpack_header(header_data)
        local req_id = header.requestId
        local content = ""
        if header.contentLength > 0 then
            content, err = recv_exact(conn, header.contentLength)
            if not content then break end
        end
        if header.paddingLength > 0 then
            if not recv_exact(conn, header.paddingLength) then break end
        end

        if header.type == C.FCGI_BEGIN_REQUEST then
            local role = unpack_u16_be(content, 1)
            local flags = string.byte(content, 3)
            if role ~= C.FCGI_RESPONDER then
                local end_body = string.char(0,0,0,0, C.FCGI_UNKNOWN_ROLE, 0,0,0)
                send_record(conn, C.FCGI_END_REQUEST, req_id, end_body)
            else
                local keep_conn = (bit.band(flags, C.FCGI_KEEP_CONN) == C.FCGI_KEEP_CONN)
                local req = Request:new(req_id, conn, keep_conn)
                requests[req_id] = req
                coroutine.wrap(function()
                    local ok, err_msg = pcall(handler, req)
                    if not ok then
                        pcall(req.stderr.write, req.stderr, "Handler error: " .. tostring(err_msg))
                        if not req.responded then req:finish(1) end
                    elseif not req.responded then
                        req:finish()
                    end
                    if not req.keep_conn then active = false end
                    requests[req_id] = nil
                end)()
            end
        elseif header.type == C.FCGI_PARAMS then
            if requests[req_id] then requests[req_id]:add_params(content) end
        elseif header.type == C.FCGI_STDIN then
            if requests[req_id] then requests[req_id]:add_stdin(content) end
        elseif header.type == C.FCGI_ABORT_REQUEST then
            if requests[req_id] then
                if not requests[req_id].responded then requests[req_id]:finish(1) end
                requests[req_id] = nil
            end
        elseif header.type == C.FCGI_GET_VALUES then
            local values = unpack_params(content)
            local results = {}
            if values["FCGI_MAX_CONNS"] ~= nil then results["FCGI_MAX_CONNS"] = tostring(opts.max_conns or 10) end
            if values["FCGI_MAX_REQS"]  ~= nil then results["FCGI_MAX_REQS"]  = tostring(opts.max_reqs or 10) end
            if values["FCGI_MPXS_CONNS"] ~= nil then results["FCGI_MPXS_CONNS"] = tostring(opts.mpxs_conns or 1) end
            send_record(conn, C.FCGI_GET_VALUES_RESULT, 0, pack_name_values(results))
        end
    end
    -- cleanup on disconnect
    for _, req in pairs(requests) do
        if not req.responded then req:finish(1) end
    end
    conn:close()
end

---
-- start server
--
-- @param opts (table) Configuration table with keys:
--   - `path` (string): Path to a Unix domain socket to listen on.
--   - `host` (string): Hostname or IP to bind to (default: "127.0.0.1").
--   - `port` (number): TCP port to listen on (default: 9000).
--   - `backlog` (number): Socket listen backlog (default: 32).
--   - `max_conns`, `max_reqs`, `mpxs_conns`: Values for FCGI_GET_VALUES.
-- @param handler (function) The function to call for each new request.
--
function M.listen(opts, handler)
    if type(opts) == 'function' then
        handler = opts
        opts = {}
    elseif type(opts) == 'string' then
        opts = { path = opts }
    end
    opts = opts or {}
    opts.host = opts.host or "127.0.0.1"
    opts.port = opts.port or 9000
    opts.backlog = opts.backlog or 32

    local server, err
    if opts.path then
        server, err = socket.bind(opts.path, nil)
    else
        server, err = socket.bind(opts.host, opts.port, opts.backlog)
    end
    if not server then
        error("Could not bind socket: " .. tostring(err))
    end

    local listen_addr = opts.path or (opts.host .. ":" .. opts.port)
    print("FastCGI server listening on " .. listen_addr)

    while true do
        local conn, err_accept = server:accept()
        if conn then
            coroutine.wrap(function()
                pcall(handle_connection, conn, handler, opts)
            end)()
        else
            io.stderr:write("Error accepting connection: " .. tostring(err_accept) .. "\n")
            break
        end
    end
end


--------------------------------------------------------------------------------
-- CLIENT
--------------------------------------------------------------------------------
local ClientConnection = {}
ClientConnection.__index = ClientConnection

function ClientConnection:new(sock)
    return setmetatable({ sock = sock }, self)
end

function ClientConnection:begin_request(req_id, role, keep_conn)
    local flags = keep_conn and C.FCGI_KEEP_CONN or 0
    local body = pack_u16_be(role) .. string.char(flags) .. string.rep('\0', 5)
    return send_record(self.sock, C.FCGI_BEGIN_REQUEST, req_id, body)
end

function ClientConnection:send_params(req_id, params_tbl)
    local body = pack_name_values(params_tbl)
    local ok, err = send_record(self.sock, C.FCGI_PARAMS, req_id, body)
    if not ok then return ok, err end
    -- Send empty record to terminate the stream
    return send_record(self.sock, C.FCGI_PARAMS, req_id, "")
end

function ClientConnection:send_stdin(req_id, data)
    data = data or ""
    local pos = 1
    while pos <= #data do
        local chunk_size = math.min(C.FCGI_MAX_CONTENT_LEN, #data - pos + 1)
        local chunk = string.sub(data, pos, pos + chunk_size - 1)
        local ok, err = send_record(self.sock, C.FCGI_STDIN, req_id, chunk)
        if not ok then return false, err end
        pos = pos + chunk_size
    end
    return true
end

function ClientConnection:close_stdin(req_id)
    return send_record(self.sock, C.FCGI_STDIN, req_id, "")
end

-- return iterator for reading response records from server
function ClientConnection:responses()
    local sock = self.sock
    local function iterator()
        local header_data, err = recv_exact(sock, C.FCGI_HEADER_LEN)
        if not header_data then return nil, err end
        local header = unpack_header(header_data)
        local content = ""
        if header.contentLength > 0 then
            content, err = recv_exact(sock, header.contentLength)
            if not content then return nil, err end
        end
        if header.paddingLength > 0 then
            local _, perr = recv_exact(sock, header.paddingLength)
            if not _ then return nil, perr end
        end
        return header.type, header.requestId, content
    end
    return iterator
end

function ClientConnection:close()
    if self.sock then self.sock:close() end
    self.sock = nil
end

---
-- connect to fcgi server
--
-- @param opts (table) Configuration table with keys:
--   - `path` (string): Path to a Unix domain socket to connect to.
--   - `host` (string): Server hostname or IP (default: "127.0.0.1").
--   - `port` (number): Server TCP port (default: 9000).
--   - `timeout` (number): Connection timeout in seconds (default: 5).
-- @return (object, string) A new client connection object on success,
--         or nil and an error message on failure.
--
function M.connect(opts)
    opts = opts or {}
    local sock, err = socket.tcp()
    if not sock then return nil, err end
    sock:settimeout(opts.timeout or 5)
    local ok
    if opts.path then
        ok, err = sock:connect(opts.path)
    else
        opts.host = opts.host or "127.0.0.1"
        opts.port = opts.port or 9000
        ok, err = sock:connect(opts.host, opts.port)
    end
    if not ok then return nil, err end
    return ClientConnection:new(sock)
end

return M
