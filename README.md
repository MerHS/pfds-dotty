## Purely Functional Data Strutures with Scala Dotty

from the book PFDS (Okasaki 1998)

It does not require any dependencies. Implemented with pure Dotty types.

### SML Signature to Dotty Trait / Type Classes

There are two implementations for each chapters, `pfds.sig.ChapN.scala` and `pfds.ChapN.scala`. The former implements each SML Signature as is (see [ML-Style Scala](https://github.com/yawaramin/scala-modules/blob/master/README.md) and [sig/Chap2.scala](src/main/scala/pfds/sig/Chap2Sig.scala)). The latter code implements each chapter with type classes according to the Haskell codes in the appendix of the book. I think the latter codes can be used with (hopefully) real Scala codes. 

Also, I modified some tuple type function parameters of the book to support currying  (according to [ML-Style Scala](https://github.com/yawaramin/scala-modules/blob/master/README.md) with piper function |>). Those change were only applied in `pfds.sig._`, and the Scala-way code handles those functions case-by-case. (does not require piper function)