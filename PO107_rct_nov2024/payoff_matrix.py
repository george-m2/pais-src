import numpy as np
import matplotlib.pyplot as plt

plt.figure(figsize=(8, 8))

data = np.array([
    [(4, 3), (1, 4)],
    [(3, 2), (0, 0)]
])

cell_text = [
    ['(4, 3)\nReform: Moderate gains\nElites: Maintain stability', '(1, 4)\nReform: No gains\nElites: Keep power'],
    ['(3, 2)\nReform: Achieved but divided\nElites: Power loss', '(0, 0)\nReform: Violence costs\nElites: Instability']
]

table = plt.table(cellText=cell_text,
                 rowLabels=['Peaceful\nReform', 'Revolution\nThreat'],
                 colLabels=['Concede', 'Repress'],
                 loc='center',
                 cellLoc='center',
                 rowLoc='center',
                 colLoc='center')

table.auto_set_font_size(False)
table.set_fontsize(9)
table.scale(1.5, 2.5)

#readability, hex is grey
for i in range(2):
    for j in range(2):
        table._cells[(i+1, j)].set_facecolor('#f0f0f0')
        
plt.axis('off')
plt.tight_layout()
plt.savefig('table.png', bbox_inches='tight', dpi=300)
