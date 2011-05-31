`k_k_util` - Kip's collection of personal utility libraries for
[Scala](http://www.scala-lang.org/).


## Highlights

#### `k_k_.algo.Consistent_Choice[T]`

Consistent hashing / consistent choice scheme for assigning each input
value in a repeatable manner to a choice of some parameterizable type, with
choices probabilistically distributed according to an associated configured
weight.


#### `k_k_.data.rand.Rand_Gen`

Abstract `Seedable` randomness generator easily tunable by a mixin policy trait.


#### `k_k_.fs.Derive_Name`

Functions for deriving a `File` path name from an existing one.


#### `k_k_.fs.Resources`

Utility functions for processing classpath resources; for example, to return
every classpath resource name beginning with a particular file path prefix.


#### `k_k_.io.data.Lines_From_Data_File`

Abstract base class for reading lines from a formatted (data) file, skipping
comments, etc.

#### `k_k_.io.data.String_Seq_From_Data_File`

Reads lines from a formatted (data) file, returning a `Seq[String]`, skipping
comments, etc.

#### `k_k_.io.data.Assoc_Seq_From_Data_File[Key_T, Val_T]`

Abstract base class for reading (key, value) pairs from a formatted (data) file,
returning a `Seq[(Key_T, Val_T)]` or a `Map[Key_T, Val_T]`, skipping comments,
etc.


#### `k_k_.io.stream.Byte_Input`

Functions for bulk handling byte-oriented input.

#### `k_k_.io.stream.Char_Input`

Functions for bulk handling character-oriented input.


#### `k_k_.value.Clamp`

Functions for constraining the range of a value.


## Examples


#### `k_k_.algo.Consistent_Choice[T]`

    {{{
    import k_k_.algo.Consistent_Choice

    // load-balance with weight favoring odd numbered handlers 2:1
    val handlers = 0 until 5 map { n => ("handler" + n,
                                         if (n % 2 == 0) 1.0 else 2.0)
        }
    // = Seq[(String, Double)](("handler0", 1.0), ("handler1", 2.0), ...)

    val choice_multiple = 100
    val choices = Consistent_Choice[String](choice_multiple) ++ handlers
    ...

    for {
      req <- req_stream
    } {
      val handler = choices.choose_for(calc_req_header_sig(req))
      Handler_Mgr.get(handler) match {
        case Some(handler_actor) => handler_actor ! req
        case None => send_err_response(req)
      }
    }
    }}}


#### `k_k_.data.rand.Rand_Gen`

    {{{
    import k_k_.data.rand._

    val rand = new Rand_Gen with Truly_Random // cryptograpically-secure
    val (n, guess, blah_blah) = (rand.gen_long,
                                 rand.gen_int_between(1, 100),
                                 rand.gen_str(5, 25))
    }}}



## License

#### Licensed under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0)

See: `src/main/resources/LICENSE`

I hope this code is helpful--share and enjoy!
