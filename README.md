# Example NLP project: HMM Noun-phrase Chunking

[Noun-phrase](https://en.wikipedia.org/wiki/Noun_phrase) chunking does a shallow syntactic parse of a text. The text is preprocessed into suitable units, such as words or parts-of-speech. The chunker then has to decide whether each unit is inside or outside of a noun phrase.

This chunker is a [hidden Markov model](https://en.wikipedia.org/wiki/Hidden_Markov_model) using [Viterbi](https://en.wikipedia.org/wiki/Viterbi_algorithm) to find the most likely sequence of states (`inside` or `outside`) given observations of part-of-speech tags. I built it in Scheme, while learning Scheme and without libraries, in order to understand as thoroughly as possible how it worked.

Data is from [the CoNLL-2000 shared task](https://www.clips.uantwerpen.be/conll2000/). The model and states were given. I designed and implemented several modifications aimed at improving performance. See `results.pdf` for the full description and results.