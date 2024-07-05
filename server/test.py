import http.server
import socketserver
import os

PORT = 8000
DIRECTORY = "static"

class SimpleHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def translate_path(self, path):
        # Modify the path to serve files from the static directory
        path = os.path.join(DIRECTORY, path.lstrip('/'))
        if os.path.isdir(path):
            # If the path is a directory, serve index.html
            path = os.path.join(path, 'index.html')
        return path

handler = SimpleHTTPRequestHandler

with socketserver.TCPServer(("", PORT), handler) as httpd:
    print(f"Serving at port {PORT}")
    httpd.serve_forever()
