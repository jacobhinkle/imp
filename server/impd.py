#!/usr/bin/env python

import argparse
import socket, sys, os

def run(socket_address='impsocket', backlog=5, size=1024):
    # Make sure the socket does not already exist
    try:
        os.unlink(socket_address)
    except OSError:
        if os.path.exists(socket_address):
            raise

    print('Starting server at {addr}'.format(addr=socket_address))
    print('Press Ctrl+C to stop')

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.bind(socket_address)

    # Listen for incoming connections
    sock.listen(1)

    while True:
        # Wait for a connection
        print('waiting for a connection',file=sys.stderr)
        connection, client_address = sock.accept()
        try:
            print('connection from {client}'.format(client=client_address),file=sys.stderr)

            # Receive the data in small chunks and retransmit it
            while True:
                data = connection.recv(100)
                print('received {data}'.format(data=data),file=sys.stderr)
                if data:
                    print('sending data back to the client',file=sys.stderr)
                    connection.sendall(data)
                else:
                    print('no more data from {client}'.format(client=client_address),file=sys.stderr)
                    break
                
        finally:
            # Clean up the connection
            connection.close()

if __name__ == '__main__':
    # parse command line args
    parser = argparse.ArgumentParser(description='IMP: a basic scientific data server')
    #parser.add_argument('--port', metavar='p', type=int, help='Port on which to listen', default=8000)
    parser.add_argument('--socket', metavar='s', type=str, help='Address of socket file', default='/tmp/impsocket')

    args = parser.parse_args()

    # set up internal data structures...


    # do the damn thing
    run(socket_address=args.socket)
