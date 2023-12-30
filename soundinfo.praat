select all
Remove
dir$="C:\features\audios\"
table2= Create Table with column names: "table2", 0, "Word F1 F1_Bark F2 F2_Bark F3 F3_Bark Duration" 
strings= Create Strings as file list: "files", dir$ + "*.wav"
nfiles= Get number of strings
for i to nfiles
	file$= Get string: i
	sound= Read from file: dir$ + file$
	tg= Read from file: dir$ + file$ - ".wav" + ".TextGrid"
	vstart= Get start time of interval: 1, 2
	vend= Get end time of interval: 1, 2
	selectObject: sound
	formant= To Formant (burg): 0, 5, 5500, 0.025, 40
	f1= Get value at time: 1, ((vend-vstart)/2)+vstart, "hertz", "linear"
	f2= Get value at time: 2, ((vend-vstart)/2)+vstart, "hertz", "linear"
	f3= Get value at time: 3, ((vend-vstart)/2)+vstart, "hertz", "linear"
	selectObject: table2
	Append row
	Set string value: i, "Word", file$ - ".wav"
	Set numeric value: i, "F1", f1
	Set numeric value: i, "F1_Bark", hertzToBark(f1)
	Set numeric value: i, "F2", f2
	Set numeric value: i, "F2_Bark", hertzToBark(f2)
	Set numeric value: i, "F3", f3
	Set numeric value: i, "F3_Bark", hertzToBark(f3)
	Set numeric value: i, "Duration", vend-vstart
	selectObject: strings
endfor
#append extra column with vowel
selectObject: table2
Append column: "vowel"
nrows=Get number of rows
for i to nrows
word$= Get value: i, "Word"
if rindex (word$, "ie") != 0 
Set string value: i, "vowel", "i"
elif rindex (word$, "ue") != 0
Set string value: i, "vowel", "y"
elif rindex (word$, "ue") != 0
Set string value: i, "vowel", "i"
elif word$=="schiff" | word$=="mitte" | word$=="still"
Set string value: i, "vowel", "\ic"
else
Set string value: i, "vowel", "u"
endif
endfor
#gets a further table with mean values per contrast
clearinfo
appendInfoLine: "vowel", tab$, "F1_mean", tab$, "F1_sd", tab$, "F1Bark_mean", tab$, "F1Bark_sd", tab$, "F2_mean", tab$, "F2_sd", tab$, "F2Bark_mean", tab$, "F2Bark_sd", tab$, "F3_mean", tab$, "F3_sd", tab$, "F3Bark_mean", tab$, "F3Bark_sd", tab$, "mean Duration", tab$, "sd duration"
vows= Create Strings from tokens: "vowels", "i \ic u y", " ,"
for i to 4
vowel$= Get string: i
selectObject: "Table table2"
table= Extract rows where column (text): "vowel", "is equal to", vowel$
f1mean= Get mean: "F1"
f2mean= Get mean: "F2"
f3mean= Get mean: "F3"
f1Bark_mean= Get mean: "F1_Bark"
f2Bark_mean= Get mean: "F2_Bark"
f3Bark_mean= Get mean: "F3_Bark"
duration= Get mean: "Duration"
f1sd= Get standard deviation: "F1"
f2sd= Get standard deviation: "F2"
f3sd= Get standard deviation: "F3"
f1Bark_sd= Get standard deviation: "F1_Bark"
f2Bark_sd= Get standard deviation: "F2_Bark"
f3Bark_sd= Get standard deviation: "F3_Bark"
dursd= Get standard deviation: "Duration"

appendInfoLine: vowel$, tab$, f1mean, tab$, f1sd, tab$, f1Bark_mean, tab$, f1Bark_sd, tab$, f2mean, tab$, f2sd, tab$, f2Bark_mean, tab$, f2Bark_sd, tab$, f3mean, tab$, f3sd, tab$, f3Bark_mean, tab$, f3Bark_sd, tab$, duration, tab$, dursd
selectObject: vows
endfor
