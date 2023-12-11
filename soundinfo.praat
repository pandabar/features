select all
Remove
dir$="/home/fernanda/Desktop/audios/"
table2= Create Table with column names: "table2", 0, "Word F1 F2 Duration" 
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
	selectObject: table2
	Append row
	Set string value: i, "Word", file$ - ".wav"
	Set numeric value: i, "F1", hertzToBark(f1)
	Set numeric value: i, "F2", hertzToBark(f2)
	Set numeric value: i, "Duration", vend-vstart
	selectObject: strings
endfor
#append extra column with vowel
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
appendInfoLine: "vowel", tab$, "F1 mean", tab$, "F1 sd", tab$, "F2 mean", tab$, "F2 sd", tab$, "mean Duration", tab$, "sd duration"
vows= Create Strings from tokens: "vowels", "i \ic u y", " ,"
for i to 4
vowel$= Get string: i
selectObject: "Table table2"
table= Extract rows where column (text): "vowel", "is equal to", vowel$
f1mean= Get mean: "F1"
f2mean= Get mean: "F2"
duration= Get mean: "Duration"
f1sd= Get standard deviation: "F1"
f2sd= Get standard deviation: "F2"
dursd= Get standard deviation: "Duration"
appendInfoLine: vowel$, tab$, f1mean, tab$, f1sd, tab$, f2mean, tab$, f2sd, tab$, duration, tab$, dursd
selectObject: vows
endfor
