import SocketServer
import subprocess

from BaseHTTPServer import BaseHTTPRequestHandler
from os import curdir, sep

class MyHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        print("Received GET request: " + self.path)
        if (self.path.endswith('/feh/prev')):
                subprocess.call('echo PREVIOUS... >> /home/pi/slideshow/web_access.log', shell=True)
                subprocess.call('export DISPLAY=:0 && xdotool key p >> /home/pi/slideshow/web_access.log', shell=True)
        if (self.path.endswith('/feh/next')):
                subprocess.call('echo NEXT... >> /home/pi/slideshow/web_access.log', shell=True)
                subprocess.call('export DISPLAY=:0 && xdotool key n >> /home/pi/slideshow/web_access.log', shell=True)
        if (self.path.endswith('/feh/pause')):
                subprocess.call('echo PAUSE... >> /home/pi/slideshow/web_access.log', shell=True)
                subprocess.call('export DISPLAY=:0 && xdotool key h >> /home/pi/slideshow/web_access.log', shell=True)

				#Open the static file requested and send it
        f = open(curdir + sep + "/index.html") 
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()
        self.wfile.write(f.read())
        f.close()

        return

httpd = SocketServer.TCPServer(("", 8090), MyHandler)
httpd.serve_forever()
