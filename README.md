```ghci
*Main> runWriterT $ run [] exampleSG "clean"
(Just (),["Trying to get to \"clean\"","Already there"])
*Main> runWriterT $ run [] exampleSG "foo"
creating /tmp/foo
(Just (),["Trying to get to \"foo\"","Not there; trying to reach dependencies...","Trying to get to \"bar\"","Not there; trying to reach dependencies...","Trying to get to \"clean\"","Already there"])
*Main> runWriterT $ run [] exampleSG "clean"
deleting /tmp/foo
(Just (),["Trying to get to \"clean\"","Not there; trying to reach dependencies...","Trying to get to \"foo\"","Already there"])
*Main> runWriterT $ run [] exampleSG "bar"
creating /tmp/foo
creating /tmp/foo/bar
(Just (),["Trying to get to \"bar\"","Not there; trying to reach dependencies...","Trying to get to \"foo\"","Not there; trying to reach dependencies...","Trying to get to \"clean\"","Already there"])
*Main> runWriterT $ run [] exampleSG "foo"
deleting /tmp/foo/bar
(Just (),["Trying to get to \"foo\"","Not there; trying to reach dependencies...","Trying to get to \"bar\"","Already there"])
*Main> runWriterT $ run [] exampleSG "foo"
(Just (),["Trying to get to \"foo\"","Already there"])
```
