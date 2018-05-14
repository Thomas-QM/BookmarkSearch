module BookmarkSearch

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Import

open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.PowerPack.Date

open Shared

open System
open System.Text.RegularExpressions

open Chessie.ErrorHandling

let defaultstate = Idle
let mutable state = defaultstate
let SetState x =
    state <- x
    browser.runtime.sendMessage (box (x |> StateUpdate))

type [<Pojo>] SearchElem = {Url:string; HTML:string option; Text:string option;}

let GetText url = async {
        try
            let! res = fetch url [Mode RequestMode.Sameorigin] |> Async.AwaitPromise
            let! restxt = res.text () |> Async.AwaitPromise
            return ok restxt
        with
            | error -> return fail error
}

let HTMLToText x =
    Regex.Replace (x, @"<(.|\n)*?>", "")

let SearchOptions keys threshold =
    createObj [
        "keys" ==> keys;
        "threshold" ==> threshold;
    ]

type [<Pojo>] FuzzyResult<'T> = {score:int; target:string; obj:'T}

let fuzzysort:obj = importDefault "fuzzysort"
[<Emit("fuzzysort.goAsync($0,$1,$2)")>]
let fsgoasync x y z :JS.Promise<FuzzyResult<_> array> = jsNative

let SearchElemArray (arr:SearchElem array) keys threshold query =
    async {
        try
            console.log "everything alright"
            let opts = (SearchOptions keys threshold)
            console.log opts
            let! x = fsgoasync query arr opts |> Async.AwaitPromise
            console.log x
            return ok (x |> Array.map (fun {obj={Url=x}} -> x))
        with | error -> return fail error.Message
    } |> AR

let ValidateSearch x =
    if x |> String.length = 0 then fail "Cannot use a string with a length of 0" else ok x

let ValidateNumber x =
    try
        x |> float |> ok
    with | error -> fail error.Message

let GetBookmarks () = async {
    let! tree = browser.bookmarks.getTree () |> Async.AwaitPromise
    let rec mapper = function
        | {BookmarkTree.url=Some x;} -> [|x|]
        | {children=Some x} -> x |>  Array.map mapper |> (function | [||] -> [||] | x -> Array.reduce Array.append x)
        | _ -> [||]
    return Array.item 0 tree |> mapper
}

let GetHistory days maxres = async {
    let options =
        createObj [
            "text" ==> ""
            "startTime" ==> DateTime.Now - (TimeSpan.FromDays days)
            "maxResults" ==> maxres
        ]
    let! history = browser.history.search options |> Async.AwaitPromise
    return history |> Array.choose (fun {url=url} -> url)
}

let Equals x y = x=y

let SearchRes {ToSearch=tosearch;Accuracy=accuracy;HistoryDays=historydays;HistoryResults=historyresults;HistoryBookmarks=historybookmarks;SearchMethod=searchmethod} = asyncTrial {
    Searching |> SetState

    let! tosearch = tosearch |> ValidateSearch
    let accuracy = accuracy |> float

    let dohistory = [0;1] |> List.exists (Equals historybookmarks)
    let dobookmarks = [0;2] |> List.exists (Equals historybookmarks)

    let dohtml = [0;2] |> List.exists (Equals searchmethod)
    let dourl = [0;1] |> List.exists (Equals searchmethod)

    let keys = Array.concat [(if dohtml then [|"Text";"HTML"|] else [||]); (if dourl then [|"Url"|] else [||])]

    let! bookmarks = async {
        if dobookmarks then
            let! bookmarks = GetBookmarks ()
            return bookmarks
        else return [||]
    }

    let! history = asyncTrial {
        if dohistory then
            let! historydays = historydays |> ValidateNumber
            let! historyres = historyresults |> ValidateNumber
            let! history = GetHistory historydays historyres
            return history
        else
            return [||]
    }

    let urls = Array.append bookmarks history |> Array.distinct

    let format x =
        Regex.Replace (x, @"\n" ," ")

    let! elems =
        match dohtml with
            | true -> asyncTrial {
                    let! response = urls |> Array.map GetText |> Async.Parallel
                    let html = response |> Array.choose (function | Pass x -> Some x | _ -> None) |> Array.map format
                    let text = html |> Array.map HTMLToText

                    return Array.zip3 urls html text |> Array.map (fun (url, html, txt) -> {Url=url; HTML=Some html; Text=Some txt})
                }
            | false -> asyncTrial {return urls |> Array.map (fun x -> {Url=x;HTML=None;Text=None})}


    let threshold = -accuracy

    let! res = SearchElemArray elems keys threshold tosearch
    console.log(res)
    return res
}

let Search data = async {
    let! res = SearchRes data |> Async.ofAsyncResult
    res |> Finished |> SetState
}

let HandleMessage x =
    match x with
        | StartSearch y ->
            y |> Search |> Async.StartAsPromise |> ignore
        | GetState ->
            SetState state
        | _ -> ()

let f = Func<obj,unit>(fun x -> HandleMessage (unbox<Message> x))
browser.runtime.onMessage.addListener f