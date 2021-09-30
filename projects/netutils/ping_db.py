import matplotlib.pyplot as plt
import matplotlib.animation as animation
import subprocess
from collections import deque
import math
from datetime import datetime, timedelta
import seaborn as sns

NUM_BARS = 20

plt.style.use("dark_background")

fig = plt.figure(figsize=(8, 3))
ax1 = fig.add_subplot(1,1,1)
fig.subplots_adjust(bottom=0.3)

def curr_time():
  now = datetime.now()
  current_time = now.strftime("%H:%M:%S")
  return current_time

timeArray = deque()
dataArray = deque([0]*NUM_BARS)

# Initialize timestamps for x-axis
time_now = datetime.now()
for i in range(NUM_BARS):
  ts = time_now - timedelta(seconds=i)
  timeArray.appendleft(ts.strftime("%H:%M:%S"))

def update_data():
  out = subprocess.Popen(["ping", "-c", "1", "google.com"], 
           stdout=subprocess.PIPE)
  response = str(out.communicate()[0])
  
  # print('****Response: ' + str(response))

  if ('time=' in response):
    t = response.find('time=')
    tt = response[t+5:]
    tt = tt[:tt.find(' ')]
    tt = math.floor(float(tt))
    # print(tt)
    timeArray.popleft()
    dataArray.popleft()
    timeArray.append(curr_time())
    dataArray.append(tt)
  
def animate(i):
  update_data()
  ax1.clear()
  plt.xticks(rotation=45)
  # ax1.set_xticklabels(timeArray, rotation = 45)
  ax1.bar(timeArray,dataArray)


ani = animation.FuncAnimation(fig, animate, interval=1000)
plt.xticks(rotation=45)
plt.show()