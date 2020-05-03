Creating sentiment lexicons:<br/>
Use PreprocessingData.R to preprocess the data and construct the sentiment lexicons. Use Extra if the TDM matrices are too large for your RAM.
- PreprocessingData.R 
- Extra.R

Use process.py to prep the input for the neural network (lexicon.py) and get the sentiment lexicon using lexiconSNN.py
- process.py (SNN)
- lexicon.py (SNN)
- lexiconSNN.py (SNN)

The constructed weighted PMI sentiment lexicons can be found in the folder Financial Sentiment Lexicons.
Please reference the paper if you use any of the financial sentiment lexicons or the code! 
Bos, T. 2020. Automatically-Building-Financial-Sentiment-Lexicons-while-Accounting-for-Negation.
