import matplotlib.pyplot as plt
plt.style.use(['seaborn', 'fivethirtyeight'])

fg_predicted = [31.78, 25.85, 23.09, 18.48,	14.43]
fg_actual = [31.66, 23.73, 20.94, 18.01, 14.43]
years = ["1", "2", "3", "4", "5"]

fig, ax = plt.subplots()

ax.plot(years, fg_predicted, label="Predicted")
ax.plot(years, fg_actual, label="Actual")
ax.legend()
plt.xlabel("Year")
plt.ylabel("FG")

for i in years:
    
    i = int(i) - 1
    jp = fg_predicted[i]
    ja= fg_actual[i]

    ax.annotate(str(jp), xy=(i,jp))
    ax.annotate(str(ja), xy=(i,ja))

plt.savefig("/home/ogreyesp/Desktop/Predictions-FG-patient341- using 5 visits of history.png", bbox_inches="tight", pad_inches=0)

plt.show()

