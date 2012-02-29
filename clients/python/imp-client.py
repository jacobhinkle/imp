#!/usr/bin/env python

import sys
import socket

if __name__ == '__main__':
    # Create a UDS socket
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)

    # Connect the socket to the port where the server is listening
    server_address = '../../server/impsocket'
    print('connecting to {addr}'.format(addr=server_address),file=sys.stderr)
    try:
        sock.connect(server_address)
    except socket.error as msg:
        print(msg, file=sys.stderr)
        sys.exit(1)

    try:
        
        # Send data
        message = 'This is the message.  It will be repeated.'
        print('sending "{msg}"'.format(msg=message),file=sys.stderr)
        sock.sendall(bytes(message,'UTF-8'))

        amount_received = 0
        amount_expected = len(message)
        
        while amount_received < amount_expected:
            data = sock.recv(100)
            amount_received += len(data)
            print('received {data}'.format(data=data),file=sys.stderr)

    finally:
        print('closing socket',file=sys.stderr)
        sock.close()
