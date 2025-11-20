# lua-fcgi

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A comprehensive, dependency-free FastCGI library for Lua. It supports Lua 5.1, 5.2, 5.3+ and LuaJIT, and includes both server and client implementations.

## Features

-   **Pure Lua:** No external C dependencies.
-   **Dependency:** Only requires `luasocket`.
-   **Cross-Version Support:** Works on Lua 5.1-5.4 and LuaJIT.
-   **Server & Client:** Full implementation of both server and client protocols.
-   **Flexible:** Listens on TCP or Unix domain sockets.
-   **Concurrent & Streaming:** The server handles multiple requests concurrently and provides true streaming for STDIN/STDOUT, making it efficient for large uploads or downloads.
-   **Robust:** Graceful handling of application errors and a smart loader for bitwise operations that prefers native or C-based implementations.

## Requirements

-   [LuaSocket](https://lunarmodules.github.io/luasocket/)

## Installation

Since this is a single-file library, simply copy `fcgi.lua` into your project and `require` it.

```lua
local fcgi = require("fcgi")
```

## Usage

### Server Example

This example starts a FastCGI server on port 9000 that responds with "Hello, World!" and echoes back the query string.

```lua
local fcgi = require("fcgi")

-- Define the request handler function
local function handler(req)
    -- req.params contains all the FCGI parameters (like a CGI environment)
    local query_string = req.params.QUERY_STRING or "none"

    -- The request object has stdout and stderr stream writers
    req.stdout:write("Status: 200 OK\r\n")
    req.stdout:write("Content-Type: text/html\r\n")
    req.stdout:write("\r\n") -- End of headers
    req.stdout:write("<h1>Hello from Lua!</h1>")
    req.stdout:write("<p>Your query string was: " .. query_string .. "</p>")

    -- req:finish() is called automatically when the handler returns,
    -- but can be called manually if needed.
end

-- Listen on a TCP socket
fcgi.listen({ host = "127.0.0.1", port = 9000 }, handler)

-- Alternatively, listen on a Unix domain socket
-- fcgi.listen({ path = "/var/run/my_app.sock" }, handler)

-- Or use the shorthand for the default port 9000
-- fcgi.listen(handler)
```

### Client Example

This example connects to a FastCGI server, sends a request, and prints the response.

```lua
local fcgi = require("fcgi")

-- Connect to the server
local conn, err = fcgi.connect({ port = 9000 })
if not conn then
    error("Could not connect: " .. tostring(err))
end

local C = fcgi.constants
local request_id = 1

-- 1. Begin the request
conn:begin_request(request_id, C.FCGI_RESPONDER, false)

-- 2. Send parameters
local params = {
    REQUEST_METHOD  = "GET",
    SCRIPT_FILENAME = "/var/www/html/index.php", -- Example for PHP-FPM
    QUERY_STRING    = "foo=bar"
}
conn:send_params(request_id, params)

-- 3. Close STDIN (since this is a GET request with no body)
conn:close_stdin(request_id)

-- 4. Read the response
print("--- Response from Server ---")
for type, req_id, content in conn:responses() do
    if req_id == request_id then
        if type == C.FCGI_STDOUT and #content > 0 then
            io.stdout:write(content)
        elseif type == C.FCGI_STDERR and #content > 0 then
            io.stderr:write("STDERR: " .. content)
        elseif type == C.FCGI_END_REQUEST then
            print("\n--- End of Request ---")
            break
        end
    end
end

-- 5. Close the connection
conn:close()
```

## License

This library is released under the MIT License. See the LICENSE file for details.
