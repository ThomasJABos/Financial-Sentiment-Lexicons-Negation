# Automatically-Building-Financial-Sentiment-Lexicons-while-Accounting-for-Negation

Here, one can find the financial sentiment lexicons that are created in the thesis Automatically-Building-Financial-Sentiment-Lexicons-while-Accounting-for-Negation (Bos, 2020) and the code that is used to create and evaluate them.

Creating sentiment lexicons:<br/>
Use PreprocessingData.R to preprocess the data and construct the sentiment lexicons. Use Extra if the TDM matrices are too large for your RAM.
- PreprocessingData.R 
- Extra.R

Use process.py to prep the input for the neural network (lexicon.py) and get the sentiment lexicon using lexiconSNN.py
- process.py (SNN)
- lexicon.py (SNN)
- lexiconSNN.py (SNN)

Evaluation of sentiment lexicons:<br/>
Use the SemEval.R code to obtain the microblogging and financial headlines data set. Prep the data with the NWPrep.R and extract the external lexicons with ExternalLexicons.R
- SemEval.R
- NWPrep.R
- ExternalLexicons.R

Use the unsupervised.R script for the unsupervised evaluation, the supervisedPrep.R and supervisedLogistic.R for the supervised evaluation, and the btc.R for the practical evaluation.
- unsupervised.R
- supervisedPrep.R
- supervisedLogistic.R
- btc.R

The constructed weighted PMI sentiment lexicons can be found in the folder Financial Sentiment Lexicons.
Please reference the paper if you use any of the financial sentiment lexicons or the code/
Bos, T. Frasincar, F. 2020. 
Automatically Building Financial Sentiment Lexicons while Accounting for Negation.

