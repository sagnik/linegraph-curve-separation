import matplotlib.pyplot as plt
import numpy as np

t = np.arange(0.0, 2.0, 0.01)
s = np.sin(2*np.pi*t)
plt.plot(t, s)

markers = {0: 'tickleft', 1: 'tickright', '3': 'tri_left', '8': 'octagon', '4': 'tri_right', 5: 'caretright', 6: 'caretup', 7: 'caretdown', 'o': 'circle', 2: 'tickup', '^': 'triangle_up', 3: 'tickdown', '>': 'triangle_right', 'd': 'thin_diamond', 'x': 'x', 'h': 'hexagon1', 'H': 'hexagon2', '': 'nothing', '|': 'vline', 'v': 'triangle_down', '*': 'star', ',': 'pixel', 'p': 'pentagon', ' ': 'nothing', '1': 'tri_down', 's': 'square', '2': 'tri_up', None: 'nothing', '+': 'plus', '<': 'triangle_left', 'None': 'nothing', '_': 'hline', 'D': 'diamond', 4: 'caretleft', '.': 'point'}

plt.xlabel('time (s)')
plt.ylabel('voltage (mV)')
plt.title('About as simple as it gets, folks')
plt.grid(True)
plt.savefig("test.svg")
#plt.show()
