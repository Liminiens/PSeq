module FSharp.Collections.ParallelSeq.Tests

open System
open System.Collections
open System
open System.Linq
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open Xunit
open Xunit.Abstractions
    
let failTest() = Assert.False(true)

let failTestWith (str: string) (output: string -> unit) =
    do output str 
    failTest()

type PSeq_Tests(output: ITestOutputHelper) =
    
    member private __.MapWithSideEffectsTester (map : (int -> int) -> seq<int> -> pseq<int>) expectExceptions =
        let i = ref 0
        let f x = i := !i + 1; x*x
        let e = ([1;2] |> map f).GetEnumerator()
        
        if expectExceptions then
            __.CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
            Assert.Equal(0, !i)
        if not (e.MoveNext()) then failTest()
        Assert.Equal(1, !i)
        let _ = e.Current
        Assert.Equal(1, !i)
        let _ = e.Current
        Assert.Equal(1, !i)
        
        if not (e.MoveNext()) then failTest()
        Assert.Equal(2, !i)
        let _ = e.Current
        Assert.Equal(2, !i)
        let _ = e.Current
        Assert.Equal(2, !i)

        if e.MoveNext() then failTest()
        Assert.Equal(2, !i)
        if expectExceptions then
            __.CheckThrowsInvalidOperationExn (fun _ -> e.Current |> ignore)
            Assert.Equal(2, !i)

        
        i := 0
        let e = ([] |> map f).GetEnumerator()
        if e.MoveNext() then failTest()
        Assert.Equal(0,!i)
        if e.MoveNext() then failTest()
        Assert.Equal(0,!i)
        
        
    member private __.MapWithExceptionTester (map : (int -> int) -> seq<int> -> pseq<int>) =
        let raiser x = if x > 0 then raise(NotSupportedException()) else x
        let raises = (map raiser [0; 1])
        __.CheckThrowsAggregateException(fun _ -> PSeq.toArray raises |> ignore)
       

    /// Check that the lamda throws an exception of the given type. Otherwise
    /// calls fail()
    member private __.CheckThrowsExn<'a when 'a :> exn> (f : unit -> unit) =
        let funcThrowsAsExpected =
            try
                let _ = f ()
                Some "no exception" // Did not throw!
            with
            | :? 'a
                -> None // Thew null ref, OK
            | exn -> Some  (exn.ToString()) // Did now throw a null ref exception!
        match funcThrowsAsExpected with
        | None -> ()
        | Some s -> failTestWith s output.WriteLine

    member private __.CheckThrowsInvalidOperationExn   f = __.CheckThrowsExn<InvalidOperationException> f
    member private __.CheckThrowsArgumentNullException f = __.CheckThrowsExn<ArgumentNullException>     f
    member private __.CheckThrowsKeyNotFoundException  f = __.CheckThrowsExn<KeyNotFoundException>      f
    member private __.CheckThrowsAggregateException    f = __.CheckThrowsExn<AggregateException>        f
    member private __.CheckThrowsArgumentException     f = __.CheckThrowsExn<ArgumentException>         f

    // Verifies two sequences are equal (same length, equiv elements)
    member __.VerifyPSeqsEqual seq1 seq2 =
        let len1 = PSeq.length seq1
        let len2 = PSeq.length seq2
        if len1 <> len2 then failTestWith (sprintf "seqs not equal length: %d and %d" len1 len2) output.WriteLine
        else
            let set1 = set seq1
            let set2 = set seq2
            if set1 <> set2 then failTestWith (sprintf "contents not the same: %A %A" set1 set2) output.WriteLine

    [<Fact>]
    member __.TestAppend() =

        // empty Seq 
        let emptySeq1 = PSeq.empty
        let emptySeq2 = PSeq.empty
        let appendEmptySeq = PSeq.append emptySeq1 emptySeq2
        let expectResultEmpty = PSeq.empty
           
        __.VerifyPSeqsEqual expectResultEmpty appendEmptySeq
          
        // Integer Seq  
        let integerSeq1:seq<int> = seq [0..4]
        let integerSeq2:seq<int> = seq [5..9]
         
        let appendIntergerSeq = PSeq.append integerSeq1 integerSeq2
       
        let expectResultInteger = seq { for i in 0..9 -> i}
        
        __.VerifyPSeqsEqual expectResultInteger appendIntergerSeq
        
        
        // String Seq
        let stringSeq1:seq<string> = seq ["1";"2"]
        let stringSeq2:seq<string> = seq ["3";"4"]
        
        let appendStringSeq = PSeq.append stringSeq1 stringSeq2
        
        let expectedResultString = seq ["1";"2";"3";"4"]
        
        __.VerifyPSeqsEqual expectedResultString appendStringSeq
        
        // null Seq
        let nullSeq1 = seq [null;null]

        let nullSeq2 =seq [null;null]

        let appendNullSeq = PSeq.append nullSeq1 nullSeq2
        
        let expectedResultNull = seq [ null;null;null;null]
        
        __.VerifyPSeqsEqual expectedResultNull appendNullSeq
        
        
    [<Fact>]
    member __.TestAverage() =
        // empty Seq 
        let emptySeq:pseq<double> = PSeq.empty<double>
        
        __.CheckThrowsInvalidOperationExn (fun () ->  PSeq.average emptySeq |> ignore)       
            
        // double Seq
        let doubleSeq:seq<double> = seq [1.0;2.2;2.5;4.3]
        
        let averageDouble = PSeq.average doubleSeq
        
        Assert.False( averageDouble <> 2.5)
        
        // float32 Seq
        let floatSeq:seq<float32> = seq [ 2.0f;4.4f;5.0f;8.6f]
        
        let averageFloat = PSeq.average floatSeq
        
        Assert.False( averageFloat <> 5.0f)
        
        // decimal Seq
        let decimalSeq:seq<decimal> = seq [ 0M;19M;19.03M]
        
        let averageDecimal = PSeq.average decimalSeq
        
        Assert.False( averageDecimal <> 12.676666666666666666666666667M )
        
        // null Seq
        let nullSeq:seq<double> = null
            
        __.CheckThrowsArgumentNullException (fun () -> PSeq.average nullSeq |> ignore) 
        
        
    [<Fact>]
    member __.TestAverageBy() =
    // empty Seq 
        let emptySeq:pseq<double> = PSeq.empty<double>
        
        __.CheckThrowsInvalidOperationExn (fun () ->  PSeq.averageBy (fun x -> x+1.0) emptySeq |> ignore)
        
        // double Seq
        let doubleSeq:seq<double> = seq [1.0;2.2;2.5;4.3]
        
        let averageDouble = PSeq.averageBy (fun x -> x-2.0) doubleSeq
        
        Assert.False( averageDouble <> 0.5 )
        
        // float32 Seq
        let floatSeq:seq<float32> = seq [ 2.0f;4.4f;5.0f;8.6f]
        
        let averageFloat = PSeq.averageBy (fun x -> x*3.3f)  floatSeq
        
        Assert.False( averageFloat <> 16.5f )
        
        // decimal Seq
        let decimalSeq:seq<decimal> = seq [ 0M;19M;19.03M]
        
        let averageDecimal = PSeq.averageBy (fun x -> x/10.7M) decimalSeq
        
        Assert.False( averageDecimal <> 1.1847352024922118380062305296M )
        
        // null Seq
        let nullSeq:seq<double> = null
            
        __.CheckThrowsArgumentNullException (fun () -> PSeq.averageBy (fun (x:double)->x+4.0) nullSeq |> ignore) 
        
    //    [<Fact>]
    //    let TestCache() =
    //        // empty Seq 
    //        let emptySeq:pseq<double> = PSeq.empty<double>
    //        
    //        let cacheEmpty = PSeq.cache emptySeq
    //        
    //        let expectedResultEmpty = PSeq.empty
    //        
    //        __.VerifyPSeqsEqual expectedResultEmpty cacheEmpty
    //               
    //        // double Seq
    //        let doubleSeq:seq<double> = seq [1.0;2.2;2.5;4.3]
    //        
    //        let cacheDouble = PSeq.cache doubleSeq
    //        
    //        __.VerifyPSeqsEqual doubleSeq cacheDouble
    //        
    //            
    //        // float32 Seq
    //        let floatSeq:seq<float32> = seq [ 2.0f;4.4f;5.0f;8.6f]
    //        
    //        let cacheFloat = PSeq.cache floatSeq
    //        
    //        __.VerifyPSeqsEqual floatSeq cacheFloat
    //        
    //        // decimal Seq
    //        let decimalSeq:seq<decimal> = seq [ 0M; 19M; 19.03M]
    //        
    //        let cacheDecimal = PSeq.cache decimalSeq
    //        
    //        __.VerifyPSeqsEqual decimalSeq cacheDecimal
    //        
    //        // null Seq
    //        let nullSeq = seq [null]
    //        
    //        let cacheNull = PSeq.cache nullSeq
    //        
    //        __.VerifyPSeqsEqual nullSeq cacheNull
    //        ()

    [<Fact>]
    member __.TestCase() =

        // integer Seq
        let integerArray = [|1;2|]
        let integerSeq = PSeq.cast integerArray
        
        let expectedIntegerSeq = seq [1;2]
        
        __.VerifyPSeqsEqual expectedIntegerSeq integerSeq
        
        // string Seq
        let stringArray = [|"a";"b"|]
        let stringSeq = PSeq.cast stringArray
        
        let expectedStringSeq = seq["a";"b"]
        
        __.VerifyPSeqsEqual expectedStringSeq stringSeq
        
        // empty Seq
        let emptySeq = PSeq.cast PSeq.empty
        let expectedEmptySeq = PSeq.empty
        
        __.VerifyPSeqsEqual expectedEmptySeq PSeq.empty
        
        // null Seq
        let nullArray = [|null;null|]
        let NullSeq = PSeq.cast nullArray
        let expectedNullSeq = seq [null;null]
        
        __.VerifyPSeqsEqual expectedNullSeq NullSeq
        
    [<Fact>]
    member __.TestChoose() =
        
        // int Seq
        let intSeq = seq [1..20]    
        let funcInt x = if (x%5=0) then Some x else None       
        let intChoosed = PSeq.choose funcInt intSeq
        let expectedIntChoosed = seq { for i = 1 to 4 do yield i*5}
        
        
       
        __.VerifyPSeqsEqual expectedIntChoosed intChoosed
        
        // string Seq
        let stringSrc = seq ["list";"List"]
        let funcString x = match x with
                            | "list"-> Some x
                            | "List" -> Some x
                            | _ -> None
        let strChoosed = PSeq.choose funcString stringSrc   
        let expectedStrChoose = seq ["list";"List"]
      
        __.VerifyPSeqsEqual expectedStrChoose strChoosed
        
        // empty Seq
        let emptySeq = PSeq.empty
        let emptyChoosed = PSeq.choose funcInt emptySeq
        
        let expectedEmptyChoose = PSeq.empty
        
        __.VerifyPSeqsEqual expectedEmptyChoose emptySeq
        

        // null Seq
        let nullSeq:seq<'a> = null    
        
        __.CheckThrowsArgumentNullException (fun () -> PSeq.choose funcInt nullSeq |> ignore) 
    
    //    [<Fact>]
    //    let TestCompare() =
    //    
    //        // int Seq
    //        let intSeq1 = seq [1;3;7;9]    
    //        let intSeq2 = seq [2;4;6;8] 
    //        let funcInt x y = if (x>y) then x else 0
    //        let intcompared = PSeq.compareWith funcInt intSeq1 intSeq2
    //       
    //        Assert.False( intcompared <> 7 )
    //        
    //        // string Seq
    //        let stringSeq1 = seq ["a"; "b"]
    //        let stringSeq2 = seq ["c"; "d"]
    //        let funcString x y = match (x,y) with
    //                             | "a", "c" -> 0
    //                             | "b", "d" -> 1
    //                             |_         -> -1
    //        let strcompared = PSeq.compareWith funcString stringSeq1 stringSeq2  
    //        Assert.False( strcompared <> 1 )
    //         
    //        // empty Seq
    //        let emptySeq = PSeq.empty
    //        let emptycompared = PSeq.compareWith funcInt emptySeq emptySeq
    //        
    //        Assert.False( emptycompared <> 0 )
    //       
    //        // null Seq
    //        let nullSeq:seq<int> = null    
    //         
    //        __.CheckThrowsArgumentNullException (fun () -> PSeq.compareWith funcInt nullSeq emptySeq |> ignore)  
    //        __.CheckThrowsArgumentNullException (fun () -> PSeq.compareWith funcInt emptySeq nullSeq |> ignore)  
    //        __.CheckThrowsArgumentNullException (fun () -> PSeq.compareWith funcInt nullSeq nullSeq |> ignore)  
    //
    //        ()
        
    [<Fact>]
    member __.TestConcat() =
        // integer Seq
        let seqInt = 
            seq { for i in 0..9 do                
                    yield seq {for j in 0..9 do
                                yield i*10+j}}
        let conIntSeq = PSeq.concat seqInt
        let expectedIntSeq = seq { for i in 0..99 do yield i}
        
        __.VerifyPSeqsEqual expectedIntSeq conIntSeq
         
        // string Seq
        let strSeq = 
            seq { for a in 'a' .. 'b' do
                    for b in 'a' .. 'b' do
                        yield seq [a; b] }
     
        let conStrSeq = PSeq.concat strSeq
        let expectedStrSeq = seq ['a';'a';'a';'b';'b';'a';'b';'b';]
        __.VerifyPSeqsEqual expectedStrSeq conStrSeq
        
        // Empty Seq
        let emptySeqs = seq [PSeq.empty;PSeq.empty;PSeq.empty;PSeq.empty]
        let conEmptySeq = PSeq.concat emptySeqs
        __.VerifyPSeqsEqual conEmptySeq Seq.empty

        // null Seq
        let nullSeq:seq<'a> = null
        
        __.CheckThrowsArgumentNullException (fun () -> PSeq.concat nullSeq  |> ignore) 
        
    [<Fact>]
    member __.TestCountBy() =
        // integer Seq
        let funcIntCount_by (x:int) = x%3 
        let seqInt = 
            seq { for i in 0..9 do                
                    yield i}
        let countIntSeq = PSeq.countBy funcIntCount_by seqInt
         
        let expectedIntSeq = seq [0,4;1,3;2,3]
        
        __.VerifyPSeqsEqual expectedIntSeq countIntSeq
         
        // string Seq
        let funcStrCount_by (s:string) = s.IndexOf("key")
        let strSeq = seq [ "key";"blank key";"key";"blank blank key"]
       
        let countStrSeq = PSeq.countBy funcStrCount_by strSeq
        let expectedStrSeq = seq [0,2;6,1;12,1]
        __.VerifyPSeqsEqual expectedStrSeq countStrSeq
        
        // Empty Seq
        let emptySeq = PSeq.empty
        let countEmptySeq = PSeq.countBy funcIntCount_by emptySeq
        let expectedEmptySeq =seq []
        
        __.VerifyPSeqsEqual expectedEmptySeq countEmptySeq  

        // null Seq
        let nullSeq:seq<'a> = null
       
        __.CheckThrowsArgumentNullException (fun () -> PSeq.countBy funcIntCount_by nullSeq  |> ignore) 
    
    [<Fact>]
    member __.TestDistinct() =
        
        // integer Seq
        let IntDistinctSeq =  
            seq { for i in 0..9 do                
                    yield i % 3 }
       
        let DistinctIntSeq = PSeq.distinct IntDistinctSeq
       
        let expectedIntSeq = seq [0;1;2]
        
        __.VerifyPSeqsEqual expectedIntSeq DistinctIntSeq
     
        // string Seq
        let strDistinctSeq = seq ["elementDup"; "ele1"; "ele2"; "elementDup"]
       
        let DistnctStrSeq = PSeq.distinct strDistinctSeq
        let expectedStrSeq = seq ["elementDup"; "ele1"; "ele2"]
        __.VerifyPSeqsEqual expectedStrSeq DistnctStrSeq

        // array Seq
        let arrDistinctSeq = seq [[|1|];[|1;2|]; [|1|];[|3|]]
       
        let DistnctArrSeq = PSeq.distinct arrDistinctSeq
        let expectedArrSeq = seq [[|1|]; [|1; 2|]; [|3|]]
        __.VerifyPSeqsEqual expectedArrSeq DistnctArrSeq
        
        
        // Empty Seq
        let emptySeq : pseq<decimal * unit>         = PSeq.empty
        let distinctEmptySeq : pseq<decimal * unit> = PSeq.distinct emptySeq
        let expectedEmptySeq : pseq<decimal * unit> = PSeq.ofList []
       
        __.VerifyPSeqsEqual expectedEmptySeq distinctEmptySeq

        // null Seq
        let nullSeq:seq<unit> = null
       
        __.CheckThrowsArgumentNullException(fun () -> PSeq.distinct nullSeq  |> ignore) 
         
    
    [<Fact>]
    member __.TestDistinctBy () =
        // integer Seq
        let funcInt x = x % 3 
        let IntDistinct_bySeq =  
            seq { for i in 0..9 do                
                    yield i }
       
        let distinct_byIntSeq = PSeq.distinctBy funcInt IntDistinct_bySeq
        
        Assert.Equal(3, PSeq.length distinct_byIntSeq )
        
        let mappedBack = distinct_byIntSeq |> PSeq.map funcInt

        let expectedIntSeq = seq [0;1;2]
        
        __.VerifyPSeqsEqual expectedIntSeq mappedBack 
             
        // string Seq
        let funcStrDistinct (s:string, _) = s.IndexOf("key")
        let strSeq = seq [ ("key", 1); ("blank key", 2); ("key dup", 1); ("blank key dup", 2)]
       
        let DistnctStrSeq = PSeq.distinctBy funcStrDistinct strSeq
        let expectedStrSeq = seq [1; 2]
        __.VerifyPSeqsEqual expectedStrSeq (PSeq.map snd DistnctStrSeq)
        
        // Empty Seq
        let emptySeq            : pseq<int> = PSeq.empty
        let distinct_byEmptySeq : pseq<int> = PSeq.distinctBy funcInt emptySeq
        let expectedEmptySeq    : pseq<int> = PSeq.ofList []
       
        __.VerifyPSeqsEqual expectedEmptySeq distinct_byEmptySeq

        // null Seq
        let nullSeq : seq<'a> = null
       
        __.CheckThrowsArgumentNullException(fun () -> PSeq.distinctBy funcInt nullSeq  |> ignore) 
    
    [<Fact>]
    member __.TestExists() =

        // Integer Seq
        let funcInt x = (x % 2 = 0) 
        let IntexistsSeq =  
            seq { for i in 0..9 do                
                    yield i}
       
        let ifExistInt = PSeq.exists funcInt IntexistsSeq
        
        Assert.True(ifExistInt) 
            
        // String Seq
        let funcStr (s:string) = s.Contains("key")
        let strSeq = seq ["key"; "blank key"]
       
        let ifExistStr = PSeq.exists funcStr strSeq
        
        Assert.True(ifExistStr)
        
        // Empty Seq
        let emptySeq = PSeq.empty
        let ifExistsEmpty = PSeq.exists funcInt emptySeq
        
        Assert.False(ifExistsEmpty)

        // null Seq
        let nullSeq:seq<'a> = null
           
        __.CheckThrowsArgumentNullException (fun () -> PSeq.exists funcInt nullSeq |> ignore)  
    
    [<Fact>]
    member __.TestExists2() =
        // Integer Seq
        let funcInt x y = (x+y)%3=0 
        let Intexists2Seq1 =  seq [1;3;7]
        let Intexists2Seq2 = seq [1;6;3]
            
        let ifExist2Int = PSeq.exists2 funcInt Intexists2Seq1 Intexists2Seq2
        Assert.True( ifExist2Int)
             
        // String Seq
        let funcStr s1 s2 = ((s1 + s2) = "CombinedString")
        let strSeq1 = seq [ "Combined"; "Not Combined"] |> PSeq.ordered
        let strSeq2 = seq [ "String";   "Other String"] |> PSeq.ordered
        let ifexists2Str = PSeq.exists2 funcStr strSeq1 strSeq2
        Assert.True(ifexists2Str)
        
        // Empty Seq
        let emptySeq = PSeq.empty
        let ifexists2Empty = PSeq.exists2 funcInt emptySeq emptySeq
        Assert.False( ifexists2Empty)
       
        // null Seq
        let nullSeq:seq<'a> = null
        __.CheckThrowsArgumentNullException (fun () -> PSeq.exists2 funcInt nullSeq nullSeq |> ignore) 
        () 
    
    
    [<Fact>]
    member __.TestFilter() =
        // integer Seq
        let funcInt x = if (x % 5 = 0) then true else false
        let IntSeq =
            seq { for i in 1..20 do
                    yield i }
                    
        let filterIntSeq = PSeq.filter funcInt IntSeq
          
        let expectedfilterInt = seq [ 5;10;15;20]
        
        __.VerifyPSeqsEqual expectedfilterInt filterIntSeq
        
        // string Seq
        let funcStr (s:string) = s.Contains("Expected Content")
        let strSeq = seq [ "Expected Content"; "Not Expected"; "Expected Content"; "Not Expected"]
        
        let filterStrSeq = PSeq.filter funcStr strSeq
        
        let expectedfilterStr = seq ["Expected Content"; "Expected Content"]
        
        __.VerifyPSeqsEqual expectedfilterStr filterStrSeq 
        
        // Empty Seq
        let emptySeq = PSeq.empty
        let filterEmptySeq = PSeq.filter funcInt emptySeq
        
        let expectedEmptySeq =seq []
       
        __.VerifyPSeqsEqual expectedEmptySeq filterEmptySeq
       
        // null Seq
        let nullSeq:seq<'a> = null
        
        __.CheckThrowsArgumentNullException (fun () -> PSeq.filter funcInt nullSeq  |> ignore) 
    
    [<Fact>]
    member __.TestFind() =
        
        // integer Seq
        let funcInt x = if (x % 5 = 0) then true else false
        let IntSeq =
            seq { for i in 1..20 do
                    yield i }
                    
        let findInt = PSeq.find funcInt IntSeq
        Assert.Equal(findInt, 5)  
             
        // string Seq
        let funcStr (s:string) = s.Contains("Expected Content")
        let strSeq = seq [ "Expected Content";"Not Expected"]
        
        let findStr = PSeq.find funcStr strSeq
        Assert.Equal(findStr, "Expected Content")
        
        // Empty Seq
        let emptySeq = PSeq.empty
        
        __.CheckThrowsInvalidOperationExn (fun () -> PSeq.find funcInt emptySeq |> ignore)
       
        // null Seq
        let nullSeq:seq<'a> = null
        __.CheckThrowsArgumentNullException (fun () -> PSeq.find funcInt nullSeq |> ignore) 
        
    
    [<Fact>]
    member __.TestFindIndex() =
        
        // integer Seq
        let digits = [1 .. 100] |> PSeq.ofList
        let idx = digits |> PSeq.findIndex (fun i -> i.ToString().Length > 1)
        Assert.Equal(idx, 9)

        // empty Seq 
        __.CheckThrowsInvalidOperationExn (fun () -> PSeq.findIndex (fun i -> true) PSeq.empty |> ignore)
         
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.findIndex (fun i -> true) null |> ignore)
    
    [<Fact>]
    member __.TestPick() =
    
        let digits = [| 1 .. 10 |] |> PSeq.ofArray
        let result = PSeq.pick (fun i -> if i > 5 then Some(i.ToString()) else None) digits
        Assert.Equal(result, "6")
        
        // Empty seq (Bugged, 4173)
        __.CheckThrowsKeyNotFoundException (fun () -> PSeq.pick (fun i -> Some('a')) ([| |] : int[]) |> ignore)

        // Null
        __.CheckThrowsArgumentNullException (fun () -> PSeq.pick (fun i -> Some(i + 0)) null |> ignore)
        
    [<Fact>]
    member __.TestFold() =
        let funcInt x y = x+y
             
        let IntSeq =
            seq { for i in 1..10 do
                    yield i}
                    
        let foldInt = PSeq.fold funcInt 1 IntSeq
        if foldInt <> 56 then failTest()
        
        // string Seq
        let funcStr (x:string) (y:string) = x+y
        let strSeq = seq ["B"; "C";  "D" ; "E"]
        let foldStr = PSeq.fold  funcStr "A" strSeq
      
        if foldStr <> "ABCDE" then failTest()
        
        
        // Empty Seq
        let emptySeq = PSeq.empty
        let foldEmpty = PSeq.fold funcInt 1 emptySeq
        if foldEmpty <> 1 then failTest()

        // null Seq
        let nullSeq:seq<'a> = null
        
        __.CheckThrowsArgumentNullException (fun () -> PSeq.fold funcInt 1 nullSeq |> ignore) 
        
    [<Fact>]
    member __.TestForAll() =

        let funcInt x  = if x%2 = 0 then true else false
        let IntSeq =
            seq { for i in 1..10 do
                    yield i*2}
        let for_allInt = PSeq.forall funcInt  IntSeq
           
        if for_allInt <> true then failTest()
        
             
        // string Seq
        let funcStr (x:string)  = x.Contains("a")
        let strSeq = seq ["a"; "ab";  "abc" ; "abcd"]
        let for_allStr = PSeq.forall  funcStr strSeq
       
        if for_allStr <> true then failTest()
        
        
        // Empty Seq
        let emptySeq = PSeq.empty
        let for_allEmpty = PSeq.forall funcInt emptySeq
        
        if for_allEmpty <> true then failTest()
        
        // null Seq
        let nullSeq:seq<'a> = null
        __.CheckThrowsArgumentNullException (fun () -> PSeq.forall funcInt  nullSeq |> ignore)  
        
    [<Fact>]
    member __.TestForAll2() =

        let funcInt x y = if (x+y)%2 = 0 then true else false
        let IntSeq =
            seq { for i in 1..10 do
                    yield i}
            |> PSeq.ordered
                    
        let for_all2Int = PSeq.forall2 funcInt  IntSeq IntSeq
           
        if for_all2Int <> true then failTest()
        
        // string Seq
        let funcStr (x:string) (y:string)  = (x+y).Length = 5
        let strSeq1 = seq ["a"; "ab";  "abc" ; "abcd"] |> PSeq.ordered
        let strSeq2 = seq ["abcd"; "abc";  "ab" ; "a"] |> PSeq.ordered
        let for_all2Str = PSeq.forall2  funcStr strSeq1 strSeq2
       
        if for_all2Str <> true then failTest()
        
        // Empty Seq
        let emptySeq = PSeq.empty
        let for_all2Empty = PSeq.forall2 funcInt emptySeq emptySeq
        
        if for_all2Empty <> true then failTest()

        // null Seq
        let nullSeq:seq<'a> = null
        
        __.CheckThrowsArgumentNullException (fun () -> PSeq.forall2 funcInt  nullSeq nullSeq |> ignore) 
        
    [<Fact>]
    member __.TestGroupBy() =
        
        let funcInt x = x%5
             
        let IntSeq =
            seq { for i in 0 .. 9 do
                    yield i }
                    
        let group_byInt = PSeq.groupBy funcInt IntSeq |> PSeq.map (fun (i, v) -> i, PSeq.toList v |> set)
        
        let expectedIntSeq = 
            seq { for i in 0..4 do
                        yield i, set [i; i+5] }
                   
        __.VerifyPSeqsEqual group_byInt expectedIntSeq
             
        // string Seq
        let funcStr (x:string) = x.Length
        let strSeq = seq ["length7"; "length 8";  "length7" ; "length  9"]
        
        let group_byStr = PSeq.groupBy  funcStr strSeq |> PSeq.map (fun (i, v) -> i, PSeq.toList v |> set)
        let expectedStrSeq = 
            seq {
                yield 7, set ["length7"; "length7"]
                yield 8, set ["length 8"]
                yield 9, set ["length  9"] }
       
        __.VerifyPSeqsEqual expectedStrSeq group_byStr


        // array keys
        let funcStr (x:string) = x.ToCharArray() |> Array.filter (fun c -> Char.IsUpper(c))
        let strSeq = seq ["Hello"; "Goodbye";  "Hello"; "How Are You?"]
        
        let group_byStr = PSeq.groupBy funcStr strSeq |> PSeq.map (fun (i, v) -> i, PSeq.toList v|> set)
        let expectedStrSeq = 
            seq {
                yield [|'H'|], set ["Hello"; "Hello"]
                yield [|'G'|], set ["Goodbye"]
                yield [|'H';'A';'Y'|], set ["How Are You?"] }
       
        __.VerifyPSeqsEqual expectedStrSeq group_byStr

        
        //        // Empty Seq
        //        let emptySeq = PSeq.empty
        //        let group_byEmpty = PSeq.groupBy funcInt emptySeq
        //        let expectedEmptySeq = seq []
        //
        //        __.VerifyPSeqsEqual expectedEmptySeq group_byEmpty
        
        // null Seq
        let nullSeq:seq<'a> = null
        __.CheckThrowsArgumentNullException (fun () -> PSeq.iter (fun _ -> ()) (PSeq.groupBy funcInt nullSeq)) 

    [<Fact>]
    member __.TestHd() =
             
        let IntSeq =
            seq { for i in 0 .. 9 do
                    yield i }
                    
        if PSeq.head IntSeq <> 0 then failTest()
                 
        // string Seq
        let strSeq = seq ["first"; "second";  "third"]
        if PSeq.head strSeq <> "first" then failTest()
         
        // Empty Seq
        let emptySeq = PSeq.empty
        __.CheckThrowsInvalidOperationExn ( fun() -> PSeq.head emptySeq)
      
        // null Seq
        let nullSeq:seq<'a> = null
        __.CheckThrowsArgumentNullException (fun () ->PSeq.head nullSeq) 
        
        
    [<Fact>]
    member __.TestInit() =

        let funcInt x = x
        let init_finiteInt = PSeq.init 9 funcInt
        let expectedIntSeq = seq [ 0..8]
      
        __.VerifyPSeqsEqual expectedIntSeq  init_finiteInt
        
             
        // string Seq
        let funcStr x = x.ToString()
        let init_finiteStr = PSeq.init 5  funcStr
        let expectedStrSeq = seq ["0";"1";"2";"3";"4"]

        __.VerifyPSeqsEqual expectedStrSeq init_finiteStr
        
        // null Seq
        let funcNull x = null
        let init_finiteNull = PSeq.init 3 funcNull
        let expectedNullSeq = seq [ null;null;null]
        
        __.VerifyPSeqsEqual expectedNullSeq init_finiteNull
        
    //    [<Fact>]
    //    let TestInitInfinite() =
    //
    //        let funcInt x = x
    //        let init_infiniteInt = PSeq.initInfinite funcInt
    //        let resultint = PSeq.find (fun x -> x =100) init_infiniteInt
    //        
    //        Assert.Equal(100,resultint)
    //        
    //             
    //        // string Seq
    //        let funcStr x = x.ToString()
    //        let init_infiniteStr = PSeq.initInfinite  funcStr
    //        let resultstr = PSeq.find (fun x -> x = "100") init_infiniteStr
    //        
    //        Assert.Equal("100",resultstr)
    //       
       
    [<Fact>]
    member __.TestIsEmpty() =
        
        //seq int
        let seqint = seq [1;2;3]
        let is_emptyInt = PSeq.isEmpty seqint
        
        Assert.False(is_emptyInt)
              
        //seq str
        let seqStr = seq["first";"second"]
        let is_emptyStr = PSeq.isEmpty  seqStr

        Assert.False(is_emptyInt)
        
        //seq empty
        let seqEmpty = PSeq.empty
        let is_emptyEmpty = PSeq.isEmpty  seqEmpty
        Assert.True(is_emptyEmpty) 
        
        //seq null
        let seqnull:seq<'a> = null
        __.CheckThrowsArgumentNullException (fun () -> PSeq.isEmpty seqnull |> ignore)
        
    [<Fact>]
    member __.TestIter() =
        //        //seq int
        //        let seqint =  seq [ 1..3]
        //        let cacheint = ref 0
        //       
        //        let funcint x = cacheint := !cacheint + x
        //        PSeq.iter funcint seqint
        //        Assert.Equal(6,!cacheint)
        //              
        //        //seq str
        //        let seqStr = seq ["first";"second"]
        //        let cachestr =ref ""
        //        let funcstr x = cachestr := !cachestr+x
        //        PSeq.iter funcstr seqStr
        //         
        //        Assert.Equal("firstsecond",!cachestr, sprintf "Not equal! firstsecond <> %A" !cachestr)
        
            // empty array    
        let emptyseq = PSeq.empty
        let resultEpt = ref 0
        PSeq.iter (fun x -> failTest()) emptyseq   

        // null seqay
        let nullseq:seq<'a> =  null
        
        __.CheckThrowsArgumentNullException (fun () -> PSeq.iter (fun x -> ()) nullseq |> ignore)  
        
    [<Fact>]
    member __.TestIter2() =
    
        //        //seq int
        //        let seqint =  seq [ 1..3]
        //        let cacheint = ref 0
        //       
        //        let funcint x y = cacheint := !cacheint + x+y
        //        PSeq.iter2 funcint seqint seqint
        //        Assert.Equal(12,!cacheint)
        //              
        //        //seq str
        //        let seqStr = seq ["first";"second"]
        //        let cachestr =ref ""
        //        let funcstr x y = cachestr := !cachestr+x+y
        //        PSeq.iter2 funcstr seqStr seqStr
        //         
        //        Assert.Equal("firstfirstsecondsecond",!cachestr)
        //        
        // empty array    
        let emptyseq = PSeq.empty
        let resultEpt = ref 0
        PSeq.iter2 (fun x y-> failTest()) emptyseq  emptyseq 

        // null seqay
        let nullseq:seq<'a> =  null
        __.CheckThrowsArgumentNullException (fun () -> PSeq.iter2 (fun x y -> ()) nullseq nullseq |> ignore)  
        
        
    [<Fact>]
    member __.TestIteri() =
    
        //        // seq int
        //        let seqint =  seq [ 1..10]
        //        let cacheint = ref 0
        //       
        //        let funcint x y = cacheint := !cacheint + x+y
        //        PSeq.iteri funcint seqint
        //        Assert.Equal(100,!cacheint)
        //              
        //        // seq str
        //        let seqStr = seq ["first";"second"]
        //        let cachestr =ref 0
        //        let funcstr (x:int) (y:string) = cachestr := !cachestr+ x + y.Length
        //        PSeq.iteri funcstr seqStr
        //         
        //        Assert.Equal(12,!cachestr)
        //        
        //         // empty array    
        //        let emptyseq = PSeq.empty
        //        let resultEpt = ref 0
        //        PSeq.iteri funcint emptyseq
        //        Assert.Equal(0,!resultEpt)

        // null seqay
        let nullseq:seq<'a> =  null
        __.CheckThrowsArgumentNullException (fun () -> PSeq.iteri (fun x i -> ()) nullseq |> ignore)  
        ()
        
    [<Fact>]
    member __.TestLength() =
        // integer seq  
        let resultInt = PSeq.length {1..8}
        if resultInt <> 8 then failTest()
        
        // string Seq    
        let resultStr = PSeq.length (seq ["Lists"; "are";  "commonly" ; "list" ])
        if resultStr <> 4 then failTest()
        
        // empty Seq     
        let resultEpt = PSeq.length PSeq.empty
        if resultEpt <> 0 then failTest()

        // null Seq
        let nullSeq:seq<'a> = null     
        __.CheckThrowsArgumentNullException (fun () -> PSeq.length  nullSeq |> ignore)  
        
        ()
        
    [<Fact>]
    member __.TestMap() =

        // integer Seq
        let funcInt x = 
                match x with
                | _ when x % 2 = 0 -> 10*x            
                | _ -> x
       
        let resultInt = PSeq.map funcInt { 1..10 }
        let expectedint = seq [1;20;3;40;5;60;7;80;9;100]
        
        __.VerifyPSeqsEqual expectedint resultInt
        
        // string Seq
        let funcStr (x:string) = x.ToLower()
        let resultStr = PSeq.map funcStr (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        let expectedSeq = seq ["lists"; "are";  "commonly" ; "list"]
        
        __.VerifyPSeqsEqual expectedSeq resultStr
        
        // empty Seq
        let resultEpt = PSeq.map funcInt PSeq.empty
        __.VerifyPSeqsEqual PSeq.empty resultEpt

        // null Seq
        let nullSeq:seq<'a> = null 
        __.CheckThrowsArgumentNullException (fun () -> PSeq.map funcStr nullSeq |> ignore)
        
        
    [<Fact>]
    member __.TestMap2() =
        // integer Seq
        let funcInt x y = x+y
        let resultInt = PSeq.map2 funcInt { 1..10 } {2..2..20} 
        let expectedint = seq [3;6;9;12;15;18;21;24;27;30]
        
        __.VerifyPSeqsEqual expectedint resultInt
        
        // string Seq
        let funcStr (x:int) (y:string) = x+y.Length
        let resultStr = PSeq.map2 funcStr (seq[3;6;9;11]) (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        let expectedSeq = seq [8;9;17;15]
        
        __.VerifyPSeqsEqual expectedSeq resultStr
        
        // empty Seq
        let resultEpt = PSeq.map2 funcInt PSeq.empty PSeq.empty
        __.VerifyPSeqsEqual PSeq.empty resultEpt

        // null Seq
        let nullSeq:seq<'a> = null 
        let validSeq = seq [1]
        __.CheckThrowsArgumentNullException (fun () -> PSeq.map2 funcInt nullSeq validSeq |> ignore)
        
        
    

    //    [<Fact>]
    //    let TestMapWithSideEffects () =
    //        this.__.MapWithSideEffectsTester PSeq.map true
        
    [<Fact>]
    member __.TestMapWithException () =
        __.MapWithExceptionTester PSeq.map

        
    //    [<Fact>]
    //    let TestSingletonCollectWithSideEffects () =
    //        this.__.MapWithSideEffectsTester (fun f-> PSeq.collect (f >> PSeq.singleton)) true
        
    [<Fact>]
    member __.TestSingletonCollectWithException () =
        __.MapWithExceptionTester (fun f-> PSeq.collect (f >> PSeq.singleton))

     
    //    [<Fact>]
    //    let TestSystemLinqSelectWithSideEffects () =
    //        this.__.MapWithSideEffectsTester (fun f s -> System.Linq.ParallelEnumerable.Select(s.AsParallel(), Func<_,_>(f))) false
    //        
    [<Fact>]
    member __.TestSystemLinqSelectWithException () =
        __.MapWithExceptionTester (fun f s -> System.Linq.ParallelEnumerable.Select(s.AsParallel(), Func<_,_>(f)))

        
    //    [<Fact>]
    //    let TestMapiWithSideEffects () =
    //        let i = ref 0
    //        let f _ x = i := !i + 1; x*x
    //        let e = ([1;2] |> PSeq.mapi f).GetEnumerator()
    //        
    //        __.CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
    //        Assert.Equal(0, !i)
    //        if not (e.MoveNext()) then fail()
    //        Assert.Equal(1, !i)
    //        let _ = e.Current
    //        Assert.Equal(1, !i)
    //        let _ = e.Current
    //        Assert.Equal(1, !i)
    //        
    //        if not (e.MoveNext()) then fail()
    //        Assert.Equal(2, !i)
    //        let _ = e.Current
    //        Assert.Equal(2, !i)
    //        let _ = e.Current
    //        Assert.Equal(2, !i)
    //        
    //        if e.MoveNext() then fail()
    //        Assert.Equal(2, !i)
    //        __.CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
    //        Assert.Equal(2, !i)
    //        
    //        i := 0
    //        let e = ([] |> PSeq.mapi f).GetEnumerator()
    //        if e.MoveNext() then fail()
    //        Assert.Equal(0,!i)
    //        if e.MoveNext() then fail()
    //        Assert.Equal(0,!i)
        
    //    [<Fact>]
    //    let TestMap2WithSideEffects () =
    //        let i = ref 0
    //        let f x y = i := !i + 1; x*x
    //        let e = (PSeq.map2 f [1;2] [1;2]).GetEnumerator()
    //        
    //        __.CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
    //        Assert.Equal(0, !i)
    //        if not (e.MoveNext()) then fail()
    //        Assert.Equal(1, !i)
    //        let _ = e.Current
    //        Assert.Equal(1, !i)
    //        let _ = e.Current
    //        Assert.Equal(1, !i)
    //        
    //        if not (e.MoveNext()) then fail()
    //        Assert.Equal(2, !i)
    //        let _ = e.Current
    //        Assert.Equal(2, !i)
    //        let _ = e.Current
    //        Assert.Equal(2, !i)
    //
    //        if e.MoveNext() then fail()
    //        Assert.Equal(2,!i)
    //        __.CheckThrowsInvalidOperationExn  (fun _ -> e.Current|>ignore)
    //        Assert.Equal(2, !i)
    //        
    //        i := 0
    //        let e = (PSeq.map2 f [] []).GetEnumerator()
    //        if e.MoveNext() then fail()
    //        Assert.Equal(0,!i)
    //        if e.MoveNext() then fail()
    //        Assert.Equal(0,!i)
        
    [<Fact>]
    member __.TestCollect() =
        // integer Seq
        let funcInt x = seq [x+1]
        let resultInt = PSeq.collect funcInt { 1..10 } 
       
        let expectedint = seq {2..11}
        
        __.VerifyPSeqsEqual expectedint resultInt
        
        // string Seq
        let funcStr (y:string) = y+"ist"
       
        let resultStr = PSeq.collect funcStr (seq ["L"])
        
        
        let expectedSeq = seq ['L';'i';'s';'t']
        
        __.VerifyPSeqsEqual expectedSeq resultStr
        
        // empty Seq
        let resultEpt = PSeq.collect funcInt PSeq.empty
        __.VerifyPSeqsEqual PSeq.empty resultEpt

        // null Seq
        let nullSeq:seq<'a> = null 
       
        __.CheckThrowsArgumentNullException (fun () -> PSeq.collect funcInt nullSeq |> ignore)
        
        
    [<Fact>]
    member __.TestMapi() =

        // integer Seq
        let funcInt x y = x+y
        let resultInt = PSeq.mapi funcInt { 10..2..20 } 
        let expectedint = seq [10;13;16;19;22;25]
        
        __.VerifyPSeqsEqual expectedint resultInt
        
        // string Seq
        let funcStr (x:int) (y:string) =x+y.Length
       
        let resultStr = PSeq.mapi funcStr (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        let expectedStr = seq [5;4;10;7]
         
        __.VerifyPSeqsEqual expectedStr resultStr
        
        // empty Seq
        let resultEpt = PSeq.mapi funcInt PSeq.empty
        __.VerifyPSeqsEqual PSeq.empty resultEpt

        // null Seq
        let nullSeq:seq<'a> = null 
       
        __.CheckThrowsArgumentNullException (fun () -> PSeq.mapi funcInt nullSeq |> ignore)
        
        
    [<Fact>]
    member __.TestMax() =
        // integer Seq
        let resultInt = PSeq.max { 10..20 } 
        Assert.Equal(20,resultInt)


        // integer64 Seq
        let resultInt64 = PSeq.max { 10L..20L } 
        Assert.Equal(20L,resultInt64)


        // float Seq
        let resultFloat = PSeq.max { 10.0..20.0 } 
        Assert.Equal(20.0,resultFloat)

        // float32 Seq
        let resultFloat32 = PSeq.max { 10.0f..20.0f } 
        Assert.Equal(20.0f,resultFloat32)

        // decimal Seq
        let resultDecimal = PSeq.max { (decimal 10)..(decimal 20) } 
        Assert.Equal((decimal 20),resultDecimal)

        // string Seq
       
        let resultStr = PSeq.max (seq ["Lists"; "Are";  "MaxString" ; "List" ])
        Assert.Equal("MaxString",resultStr)
          
        // empty Seq
        __.CheckThrowsInvalidOperationExn (fun () -> PSeq.max ( PSeq.empty : pseq<float>) |> ignore)
        
        // null Seq
        let nullSeq:seq<float> = null 
        __.CheckThrowsArgumentNullException (fun () -> PSeq.max nullSeq |> ignore)
        
        
    [<Fact>]
    member __.TestMaxBy() =
    
        // integer Seq
        let funcInt x = - (x % 18)
        let resultInt = PSeq.maxBy funcInt { 2..2..20 } 
        Assert.Equal(18,resultInt)
        
        // string Seq
        let funcStr (x:string)  = x.Length 
        let resultStr = PSeq.maxBy funcStr (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        Assert.Equal("Commonly",resultStr)
         
        // empty Seq
        __.CheckThrowsInvalidOperationExn (fun () -> PSeq.maxBy funcInt (PSeq.empty : pseq<int>) |> ignore)
        
        // null Seq
        let nullSeq:seq<int> = null 
        __.CheckThrowsArgumentNullException (fun () ->PSeq.maxBy funcInt nullSeq |> ignore)
        
        
    [<Fact>]
    member __.TestMinBy() =
    
        // integer Seq
        let funcInt x = decimal(x % 18)
        let resultInt = PSeq.minBy funcInt { 2..2..20 } 
        Assert.Equal(18,resultInt)
        
        // string Seq
        let funcStr (x:string)  = x.Length 
        let resultStr = PSeq.minBy funcStr (seq ["Lists"; "Are";  "Commonly" ; "List" ])
        Assert.Equal("Are",resultStr)
          
        // empty Seq
        __.CheckThrowsInvalidOperationExn (fun () -> PSeq.minBy funcInt (PSeq.empty : pseq<int>) |> ignore) 
        
        // null Seq
        let nullSeq:seq<int> = null 
        __.CheckThrowsArgumentNullException (fun () ->PSeq.minBy funcInt nullSeq |> ignore)
        
        
          
    [<Fact>]
    member __.TestMin() =

        // integer Seq
        let resultInt = PSeq.min { 10..20 } 
        Assert.Equal(10,resultInt)


        // integer64 Seq
        let resultInt64 = PSeq.min { 10L..20L } 
        Assert.Equal(10L,resultInt64)


        // float Seq
        let resultFloat = PSeq.min { 10.0..20.0 } 
        Assert.Equal(10.0,resultFloat)

        // float32 Seq
        let resultFloat32 = PSeq.min { 10.0f..20.0f } 
        Assert.Equal(10.0f,resultFloat32)

        // decimal Seq
        let resultDecimal = PSeq.min { (decimal 10)..(decimal 20) } 
        Assert.Equal((decimal 10),resultDecimal)

        
        //        // string Seq
        //        let resultStr = PSeq.min (seq ["Lists"; "Are";  "minString" ; "List" ])
        //        Assert.Equal("Are",resultStr)
          
        // empty Seq
        __.CheckThrowsInvalidOperationExn (fun () -> PSeq.min (PSeq.empty : pseq<int>) |> ignore) 
        
        // null Seq
        let nullSeq:seq<float> = null 
        __.CheckThrowsArgumentNullException (fun () -> PSeq.min nullSeq |> ignore)
        
        
    [<Fact>]
    member __.TestNth() =
         
        // Negative index
        for i = -1 downto -10 do
            __.CheckThrowsArgumentException (fun () -> PSeq.nth i { 10 .. 20 } |> ignore)
            
        // Out of range
        for i = 11 to 20 do
            __.CheckThrowsArgumentException (fun () -> PSeq.nth i { 10 .. 20 } |> ignore)
         
            // integer Seq
        let resultInt = PSeq.nth 3 { 10..20 } 
        Assert.Equal(13, resultInt)
        
        // string Seq
        let resultStr = PSeq.nth 3 (seq ["Lists"; "Are";  "nthString" ; "List" ])
        Assert.Equal("List",resultStr)
          
        // empty Seq
        __.CheckThrowsArgumentException(fun () -> PSeq.nth 0 (PSeq.empty : pseq<decimal>) |> ignore)
       
        // null Seq
        let nullSeq:seq<'a> = null 
        __.CheckThrowsArgumentNullException (fun () ->PSeq.nth 3 nullSeq |> ignore)
       
         
    [<Fact>]
    member __.TestOf_Array() =
            // integer Seq
        let resultInt = PSeq.ofArray [|1..10|]
        let expectedInt = {1..10}
         
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr = PSeq.ofArray [|"Lists"; "Are";  "ofArrayString" ; "List" |]
        let expectedStr = seq ["Lists"; "Are";  "ofArrayString" ; "List" ]
        __.VerifyPSeqsEqual expectedStr resultStr
          
        // empty Seq 
        let resultEpt = PSeq.ofArray [| |] 
        __.VerifyPSeqsEqual resultEpt PSeq.empty
       
        
    [<Fact>]
    member __.TestOf_List() =
        // integer Seq
        let resultInt = PSeq.ofList [1..10]
        let expectedInt = {1..10}
         
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq      
        let resultStr =PSeq.ofList ["Lists"; "Are";  "ofListString" ; "List" ]
        let expectedStr = seq ["Lists"; "Are";  "ofListString" ; "List" ]
        __.VerifyPSeqsEqual expectedStr resultStr
          
        // empty Seq 
        let resultEpt = PSeq.ofList [] 
        __.VerifyPSeqsEqual resultEpt PSeq.empty
        
          
    //    [<Fact>]
    //    let TestPairwise() =
    //         // integer Seq
    //        let resultInt = PSeq.pairwise {1..3}
    //       
    //        let expectedInt = seq [1,2;2,3]
    //         
    //        __.VerifyPSeqsEqual expectedInt resultInt
    //        
    //        // string Seq
    //        let resultStr =PSeq.pairwise ["str1"; "str2";"str3" ]
    //        let expectedStr = seq ["str1","str2";"str2","str3"]
    //        __.VerifyPSeqsEqual expectedStr resultStr
    //          
    //        // empty Seq 
    //        let resultEpt = PSeq.pairwise [] 
    //        __.VerifyPSeqsEqual resultEpt PSeq.empty
    //       
    //        ()
        
    [<Fact>]
    member __.TestReduce() =
         
        // integer Seq
        let resultInt = PSeq.reduce (fun x y -> x + y) (seq [5;4;3;2;1])
        Assert.Equal(15,resultInt)
        
        //        // string Seq
        //        let resultStr = PSeq.reduce (fun (x:string) (y:string) -> x.Remove(0,y.Length)) (seq ["ABCDE";"A"; "B";  "C" ; "D" ])
        //        Assert.Equal("E",resultStr) 
       
        // empty Seq 
        __.CheckThrowsInvalidOperationExn(fun () -> PSeq.reduce (fun x y -> x/y)  PSeq.empty |> ignore)
        
        // null Seq
        let nullSeq : seq<'a> = null
        __.CheckThrowsArgumentNullException (fun () -> PSeq.reduce (fun (x:string) (y:string) -> x.Remove(0,y.Length))  nullSeq  |> ignore)   

         
    //    [<Fact>]
    //    let TestScan() =
    //        // integer Seq
    //        let funcInt x y = x+y
    //        let resultInt = PSeq.scan funcInt 9 {1..10}
    //        let expectedInt = seq [9;10;12;15;19;24;30;37;45;54;64]
    //        __.VerifyPSeqsEqual expectedInt resultInt
    //        
    //        // string Seq
    //        let funcStr x y = x+y
    //        let resultStr =PSeq.scan funcStr "x" ["str1"; "str2";"str3" ]
    //        
    //        let expectedStr = seq ["x";"xstr1"; "xstr1str2";"xstr1str2str3"]
    //        __.VerifyPSeqsEqual expectedStr resultStr
    //          
    //        // empty Seq 
    //        let resultEpt = PSeq.scan funcInt 5 PSeq.empty 
    //       
    //        __.VerifyPSeqsEqual resultEpt (seq [ 5])
    //       
    //        // null Seq
    //        let seqNull:seq<'a> = null
    //        __.CheckThrowsArgumentNullException(fun() -> PSeq.scan funcInt 5 seqNull |> ignore)
    //        ()
        
    [<Fact>]
    member __.TestSingleton() =
        // integer Seq
        let resultInt = PSeq.singleton 1
       
        let expectedInt = seq [1]
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =PSeq.singleton "str1"
        let expectedStr = seq ["str1"]
        __.VerifyPSeqsEqual expectedStr resultStr
         
        // null Seq
        let resultNull = PSeq.singleton null
        let expectedNull = seq [null]
        __.VerifyPSeqsEqual expectedNull resultNull
    
        
    [<Fact>]
    member __.TestSkip() =
    
        // integer Seq
        let resultInt = PSeq.skip 2 (seq [1;2;3;4])
        let expectedInt = seq [3;4]
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =PSeq.skip 2 (seq ["str1";"str2";"str3";"str4"])
        let expectedStr = seq ["str3";"str4"]
        __.VerifyPSeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = PSeq.skip 0 PSeq.empty 
        __.VerifyPSeqsEqual resultEpt PSeq.empty
        
         
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.skip 1 null |> ignore)
        ()
       
    [<Fact>]
    member __.TestSkip_While() =
    
        // integer Seq
        let funcInt x = (x < 3)
        let resultInt = PSeq.skipWhile funcInt (seq [1;2;3;4;5;6])
        let expectedInt = seq [3;4;5;6]
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq
        let funcStr (x:string) = x.Contains(".")
        let resultStr =PSeq.skipWhile funcStr (seq [".";"asdfasdf.asdfasdf";"";"";"";"";"";"";"";"";""])
        let expectedStr = seq ["";"";"";"";"";"";"";"";""]
        __.VerifyPSeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = PSeq.skipWhile funcInt PSeq.empty 
        __.VerifyPSeqsEqual resultEpt PSeq.empty
        
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.skipWhile funcInt null |> ignore)
        ()
       
    [<Fact>]
    member __.TestSort() =

        // integer Seq
        let resultInt = PSeq.sort (seq [1;3;2;4;6;5;7])
        let expectedInt = {1..7}
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq
       
        let resultStr =PSeq.sort (seq ["str1";"str3";"str2";"str4"])
        let expectedStr = seq ["str1";"str2";"str3";"str4"]
        __.VerifyPSeqsEqual expectedStr resultStr

        // array Seq
       
        let resultArray =PSeq.sort (seq [[|1;2|]; [|5|]; [|3;4|]; [|4|]])
        let expectedArray = seq [[|4|]; [|5|]; [|1; 2|]; [|3; 4|]]
        __.VerifyPSeqsEqual expectedArray resultArray
        
        // empty Seq 
        let resultEpt = PSeq.sort PSeq.empty 
        __.VerifyPSeqsEqual resultEpt PSeq.empty
         
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.sort null  |> ignore)
        ()
        
    [<Fact>]
    member __.TestSortBy() =

        // integer Seq
        let funcInt x = Math.Abs(x-5)
        let resultInt = PSeq.sortBy funcInt (seq [1;2;4;5;7])
        let expectedInt = seq [5;4;7;2;1]
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq
        let funcStr (x:string) = x.IndexOf("key")
        let resultStr =PSeq.sortBy funcStr (seq ["st(key)r";"str(key)";"s(key)tr";"(key)str"])
        
        let expectedStr = seq ["(key)str";"s(key)tr";"st(key)r";"str(key)"]
        __.VerifyPSeqsEqual expectedStr resultStr
        
        // array Seq
        let resultArray =PSeq.sortBy (Array.toList) (seq [[|1;2|]; [|5|]; [|3;4|]; [|4|]])
        let expectedArray = seq [[|4|]; [|5|]; [|1; 2|]; [|3; 4|]]
        __.VerifyPSeqsEqual expectedArray resultArray

        // empty Seq 
        let resultEpt = PSeq.sortBy funcInt PSeq.empty 
        __.VerifyPSeqsEqual resultEpt PSeq.empty
         
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.sortBy funcInt null  |> ignore)
        
    [<Fact>]
    member __.TestSum() =   
        // integer Seq
        let resultInt = PSeq.sum (seq [1..10])
        Assert.Equal(55,resultInt)

        // int64 Seq
        let resultInt64 = PSeq.sum (seq [1L..10L])
        Assert.Equal(55L,resultInt64)
        
        // float32 Seq
        let floatSeq = (seq [ 1.2f;3.5f;6.7f ])
        let resultFloat = PSeq.sum floatSeq
        if resultFloat <> 11.4f then failTest()
        
        // double Seq
        let doubleSeq = (seq [ 1.0;8.0 ])
        let resultDouble = PSeq.sum doubleSeq
        if resultDouble <> 9.0 then failTest()
        
        // decimal Seq
        let decimalSeq = (seq [ 0M;19M;19.03M ])
        let resultDecimal = PSeq.sum decimalSeq
        if resultDecimal <> 38.03M then failTest()      
          
      
        // empty float32 Seq
        let emptyFloatSeq = PSeq.empty<System.Single> 
        let resultEptFloat = PSeq.sum emptyFloatSeq 
        if resultEptFloat <> 0.0f then failTest()
        
        // empty double Seq
        let emptyDoubleSeq = PSeq.empty<System.Double> 
        let resultDouEmp = PSeq.sum emptyDoubleSeq 
        if resultDouEmp <> 0.0 then failTest()
        
        // empty decimal Seq
        let emptyDecimalSeq = PSeq.empty<System.Decimal> 
        let resultDecEmp = PSeq.sum emptyDecimalSeq 
        if resultDecEmp <> 0M then failTest()
        
    [<Fact>]
    member __.TestSumBy() =

        // integer Seq
        let resultInt = PSeq.sumBy (fun x -> x + 1) (seq [1..10])
        Assert.Equal(65,resultInt)
        
        // int64 Seq
        let resultInt64 = PSeq.sumBy int (seq [1L..10L])
        Assert.Equal(55,resultInt64)

        // float32 Seq
        let floatSeq = (seq [ 1.2f;3.5f;6.7f ])
        let resultFloat = PSeq.sumBy float32 floatSeq
        if resultFloat <> 11.4f then failTest()
        
        // double Seq
        let doubleSeq = (seq [ 1.0;8.0 ])
        let resultDouble = PSeq.sumBy double doubleSeq
        if resultDouble <> 9.0 then failTest()
        
        // decimal Seq
        let decimalSeq = (seq [ 0M;19M;19.03M ])
        let resultDecimal = PSeq.sumBy decimal decimalSeq
        if resultDecimal <> 38.03M then failTest()      

        // empty float32 Seq
        let emptyFloatSeq = PSeq.empty<System.Single> 
        let resultEptFloat = PSeq.sumBy float32 emptyFloatSeq 
        if resultEptFloat <> 0.0f then failTest()
        
        // empty double Seq
        let emptyDoubleSeq = PSeq.empty<System.Double> 
        let resultDouEmp = PSeq.sumBy double emptyDoubleSeq 
        if resultDouEmp <> 0.0 then failTest()
        
        // empty decimal Seq
        let emptyDecimalSeq = PSeq.empty<System.Decimal> 
        let resultDecEmp = PSeq.sumBy decimal emptyDecimalSeq 
        if resultDecEmp <> 0M then failTest()
        
    //    [<Fact>]
    //    let TestTake() =
    //        // integer Seq
    //        
    //        let resultInt = PSeq.take 3 (seq [1;2;4;5;7])
    //       
    //        let expectedInt = seq [1;2;4]
    //        __.VerifyPSeqsEqual expectedInt resultInt
    //        
    //        // string Seq
    //       
    //        let resultStr =PSeq.take 2(seq ["str1";"str2";"str3";"str4"])
    //     
    //        let expectedStr = seq ["str1";"str2"]
    //        __.VerifyPSeqsEqual expectedStr resultStr
    //        
    //        // empty Seq 
    //        let resultEpt = PSeq.take 0 PSeq.empty 
    //      
    //        __.VerifyPSeqsEqual resultEpt PSeq.empty
    //        
    //         
    //        // null Seq
    //        __.CheckThrowsArgumentNullException(fun() -> PSeq.take 1 null |> ignore)
    //        ()
        
    [<Fact>]
    member __.TesttakeWhile() =
        // integer Seq
        let funcInt x = (x < 6)
        let resultInt = PSeq.takeWhile funcInt (seq [1;2;4;5;6;7])
      
        let expectedInt = seq [1;2;4;5]
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq
        let funcStr (x:string) = (x.Length < 4)
        let resultStr =PSeq.takeWhile funcStr (seq ["a"; "ab"; "abc"; "abcd"; "abcde"])
      
        let expectedStr = seq ["a"; "ab"; "abc"]
        __.VerifyPSeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = PSeq.takeWhile funcInt PSeq.empty 
        __.VerifyPSeqsEqual resultEpt PSeq.empty
        
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.takeWhile funcInt null |> ignore)
        
    [<Fact>]
    member __.TestTo_Array() =
        // integer Seq
        let resultInt = PSeq.toArray(seq [1;2;4;5;7])
     
        let expectedInt = [|1;2;4;5;7|]
        Assert.Equal<int[]>(expectedInt, resultInt)
        
        // string Seq
        let resultStr = PSeq.toArray (seq ["str1";"str2";"str3"])
    
        let expectedStr = [|"str1";"str2";"str3"|]
        Assert.Equal<string[]>(expectedStr, resultStr)
        
        // empty Seq 
        let resultEpt = PSeq.toArray PSeq.empty<string> 
        Assert.Equal<string[]>(Array.empty<string>, resultEpt)
        
         
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.toArray null |> ignore)
    
    [<Fact>]
    member __.TestTo_List() =
        // integer Seq
        let resultInt = PSeq.toList (seq [1;2;4;5;7])
        let expectedInt = [1;2;4;5;7]
        Assert.Equal<list<int>>(expectedInt,resultInt)
        
        // string Seq
        let resultStr =PSeq.toList (seq ["str1";"str2";"str3"])
        let expectedStr =  ["str1";"str2";"str3"]
        Assert.Equal<list<string>>(expectedStr,resultStr)
        
        // empty Seq 
        let resultEpt = PSeq.toList PSeq.empty
        Assert.Equal<list<obj>>(List.empty<obj>, resultEpt)
         
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.toList null |> ignore)
        ()
        
    [<Fact>]
    member __.TestTruncate() =
        // integer Seq
        let resultInt = PSeq.truncate 3 (seq [1;2;4;5;7])
        let expectedInt = [1;2;4]
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =PSeq.truncate 2 (seq ["str1";"str2";"str3"])
        let expectedStr =  ["str1";"str2"]
        __.VerifyPSeqsEqual expectedStr resultStr
        
        // empty Seq 
        let resultEpt = PSeq.truncate 0 PSeq.empty
        __.VerifyPSeqsEqual PSeq.empty resultEpt
        
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.truncate 1 null |> ignore)
        
    [<Fact>]
    member __.TesttryFind() =
        // integer Seq
        let resultInt = PSeq.tryFind (fun x -> (x%2=0)) (seq [1;2;4;5;7])
        Assert.Equal(Some(2), resultInt)
        
            // integer Seq - None
        let resultInt = PSeq.tryFind (fun x -> (x%2=0)) (seq [1;3;5;7])
        Assert.Equal(None, resultInt)
        
        // string Seq
        let resultStr = PSeq.tryFind (fun (x:string) -> x.Contains("2")) (seq ["str1";"str2";"str3"])
        Assert.Equal(Some("str2"),resultStr)
        
        // string Seq - None
        let resultStr = PSeq.tryFind (fun (x:string) -> x.Contains("2")) (seq ["str1";"str4";"str3"])
        Assert.Equal(None,resultStr)
       
        
        // empty Seq 
        let resultEpt = PSeq.tryFind (fun x -> (x%2=0)) PSeq.empty
        Assert.Equal(None,resultEpt)

        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.tryFind (fun x -> (x%2=0))  null |> ignore)
        
    [<Fact>]
    member __.TestTryFindIndex() =

        // integer Seq
        let resultInt = PSeq.tryFindIndex (fun x -> (x % 5 = 0)) [8; 9; 10]
        Assert.Equal(Some(2), resultInt)
        
        // integer Seq - None
        let resultInt = PSeq.tryFindIndex (fun x -> (x % 5 = 0)) [9;3;11]
        Assert.Equal(None, resultInt)
        
        // string Seq
        let resultStr = PSeq.tryFindIndex (fun (x:string) -> x.Contains("2")) ["str1"; "str2"; "str3"]
        Assert.Equal(Some(1),resultStr)
        
            // string Seq - None
        let resultStr = PSeq.tryFindIndex (fun (x:string) -> x.Contains("2")) ["str1"; "str4"; "str3"]
        Assert.Equal(None,resultStr)
       
        
        // empty Seq 
        let resultEpt = PSeq.tryFindIndex (fun x -> (x%2=0)) PSeq.empty
        Assert.Equal(None, resultEpt)
        
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.tryFindIndex (fun x -> (x % 2 = 0))  null |> ignore)
        ()
        
    //    [<Fact>]
    //    let TestUnfold() =
    //        // integer Seq
    //        
    //        let resultInt = PSeq.unfold (fun x -> if x = 1 then Some(7,2) else  None) 1
    //        
    //        __.VerifyPSeqsEqual (seq [7]) resultInt
    //          
    //        // string Seq
    //        let resultStr =PSeq.unfold (fun (x:string) -> if x.Contains("unfold") then Some("a","b") else None) "unfold"
    //        __.VerifyPSeqsEqual (seq ["a"]) resultStr
    //        ()
    //        
        
    //    [<Fact>]
    //    let TestWindowed() =
    //        // integer Seq
    //        let resultInt = PSeq.windowed 5 (seq [1..10])
    //        let expectedInt = 
    //            seq { for i in 1..6 do
    //                    yield [| i; i+1; i+2; i+3; i+4 |] }
    //        __.VerifyPSeqsEqual expectedInt resultInt
    //        
    //        // string Seq
    //        let resultStr =PSeq.windowed 2 (seq ["str1";"str2";"str3";"str4"])
    //        let expectedStr = seq [ [|"str1";"str2"|];[|"str2";"str3"|];[|"str3";"str4"|]]
    //        __.VerifyPSeqsEqual expectedStr resultStr
    //      
    //        // empty Seq 
    //        let resultEpt = PSeq.windowed 2 PSeq.empty
    //        __.VerifyPSeqsEqual PSeq.empty resultEpt
    //          
    //        // null Seq
    //        __.CheckThrowsArgumentNullException(fun() -> PSeq.windowed 2 null |> ignore)
    //        ()
        
    [<Fact>]
    member __.TestZip() =
    
        // integer Seq
        let resultInt = PSeq.zip (seq [1..7]) (seq [11..17])
        let expectedInt = 
            seq { for i in 1..7 do
                    yield i, i+10 }
        __.VerifyPSeqsEqual expectedInt resultInt
        
        // string Seq
        let resultStr =PSeq.zip (seq ["str3";"str4"]) (seq ["str1";"str2"])
        let expectedStr = seq ["str3","str1";"str4","str2"]
        __.VerifyPSeqsEqual expectedStr resultStr
      
        // empty Seq 
        let resultEpt = PSeq.zip PSeq.empty PSeq.empty
        __.VerifyPSeqsEqual PSeq.empty resultEpt
          
        // null Seq
        __.CheckThrowsArgumentNullException(fun() -> PSeq.zip null null |> ignore)
        __.CheckThrowsArgumentNullException(fun() -> PSeq.zip null (seq [1..7]) |> ignore)
        __.CheckThrowsArgumentNullException(fun() -> PSeq.zip (seq [1..7]) null |> ignore)
        
    //    [<Fact>]
    //    let TestZip3() =
    //        // integer Seq
    //        let resultInt = PSeq.zip3 (seq [1..7]) (seq [11..17]) (seq [21..27])
    //        let expectedInt = 
    //            seq { for i in 1..7 do
    //                    yield i, (i + 10), (i + 20) }
    //        __.VerifyPSeqsEqual expectedInt resultInt
    //        
    //        // string Seq
    //        let resultStr =PSeq.zip3 (seq ["str1";"str2"]) (seq ["str11";"str12"]) (seq ["str21";"str22"])
    //        let expectedStr = seq ["str1","str11","str21";"str2","str12","str22" ]
    //        __.VerifyPSeqsEqual expectedStr resultStr
    //      
    //        // empty Seq 
    //        let resultEpt = PSeq.zip3 PSeq.empty PSeq.empty PSeq.empty
    //        __.VerifyPSeqsEqual PSeq.empty resultEpt
    //          
    //        // null Seq
    //        __.CheckThrowsArgumentNullException(fun() -> PSeq.zip3 null null null |> ignore)
    //        __.CheckThrowsArgumentNullException(fun() -> PSeq.zip3 null (seq [1..7]) (seq [1..7]) |> ignore)
    //        __.CheckThrowsArgumentNullException(fun() -> PSeq.zip3 (seq [1..7]) null (seq [1..7]) |> ignore)
    //        __.CheckThrowsArgumentNullException(fun() -> PSeq.zip3 (seq [1..7]) (seq [1..7]) null |> ignore)
    //        ()
        
    //    [<Fact>]
    //    let TesttryPick() =
    //         // integer Seq
    //        let resultInt = PSeq.tryPick (fun x-> if x = 1 then Some("got") else None) (seq [1..5])
    //         
    //        Assert.Equal(Some("got"),resultInt)
    //        
    //        // string Seq
    //        let resultStr = PSeq.tryPick (fun x-> if x = "Are" then Some("got") else None) (seq ["Lists"; "Are"])
    //        Assert.Equal(Some("got"),resultStr)
    //        
    //        // empty Seq   
    //        let resultEpt = PSeq.tryPick (fun x-> if x = 1 then Some("got") else None) PSeq.empty
    //        Assert.IsNull(resultEpt)
    //       
    //        // null Seq
    //        let nullSeq : seq<'a> = null 
    //        let funcNull x = Some(1)
    //        
    //        __.CheckThrowsArgumentNullException(fun () -> PSeq.tryPick funcNull nullSeq |> ignore)
    //   
    //        ()