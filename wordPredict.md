wordPredict
========================================================
author: Ian Donaldson
date: December 14th, 2014


wordPredict the shinyapp
========================================================

You can try it here: https://iandonaldson.shinyapps.io/wordPredict/

- Enter one or more words in the text box on the left.
- wordPredict will display the predicted next word on the right 
- new words are predicted in real time as you continue typing
- try entering a word and then type in words that are suggested by wordPredict
- see where it takes you!


wordPredict the algorithm
========================================================

The text entered by the user is pre-processed using the same set of regular expressions that were used to pre-process the corpus.

If the user enters:      Now iS ... :) the "time"  

it becomes:              now is the time  

and this quadragram is compared to an index of all pentagrams that were observed in the training corpus and the word that most frequently occurs after this quadragram is returned.

If the user enters fewer words (say n words), then the corresponding (n+1)gram table is consulted.  The result is returned for the longest matched n-gram that was observed at least 5 times.  




wordPredict the index
========================================================

40% of the blogs.txt corpus was used as a training set.
The index of all unigrams up to pentagrams was generated in about 1 hour and had over 15000 pentagrams that had been observed at least 5 times.  
This index is 15 MB  

see the index.R file for code (link at the end of the presentation)   

An optimization was found that sped up the slowest step of the indexing step by 300 fold - this involved using the fast aggregation capabilities of the data.table package.

Unfortunately, there was not enough time to implement these code changes and test them.  This optimization would allow much larger corpus sizes to be generated, tested and compared.  


wordPredict more information
========================================================
 
More about the corpus and an interim report on the analysis can be found here:  https://rpubs.com/ian/41353  
 
You can find code here:
https://github.com/iandonaldson/wordPredict
This is not all the code, but it will allow you to reproduce the application and index.  

The app is here: 
https://iandonaldson.shinyapps.io/wordPredict/

Thanks




