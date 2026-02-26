# Byte Pair Compression
A simple compression algorithm.

Any given file can be decomposed into an array of bytes, which is to say numbers between `0..255`. The following algorithm is run to compress the file:
- Identify most common byte pair
- Take lowest unused number and assign it to most common pair
- Repeat until there are no unused numbers or no most popular pair.

This algorithm is pretty effective at compressing real world text. A big part of why it works so well in these cases is the large amount of unused characters in most text, for example the interval 0..31.