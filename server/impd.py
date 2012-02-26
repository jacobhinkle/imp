#!/usr/bin/env python

import argparse
import http.server

class impHandler(http.server.BaseHTTPRequestHandler):
    """
    The basic request handler for imp
    """
    
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(bytes("I heard your GET request but I'm a dumb imp and don't know what to do with it","UTF-8"))
        return

def run(server_class=http.server.HTTPServer, handler_class=http.server.BaseHTTPRequestHandler, port=8000):
    server_address = ('', port)

    print('Starting server on port {port}'.format(port=port))

    httpd = server_class(server_address, handler_class)
    httpd.serve_forever()

if __name__ == '__main__':
    # parse command line args
    parser = argparse.ArgumentParser(description='IMP: a basic scientific data server')
    parser.add_argument('--port', metavar='p', type=int, help='Port on which to listen', default=8000)

    args = parser.parse_args()

    # set up internal data structures...

    # do the damn thing
    run(handler_class=impHandler, port=args.port)
