module Popup

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Import

open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.PowerPack.Date

open System
open System.Text.RegularExpressions

open Shared

open Chessie.ErrorHandling

importAll "./App.scss"

let getvalue (x:obj) =
    (x :?> HTMLInputElement).value

let getselectvalue (x:obj) =
    (x :?> HTMLSelectElement).value

let InitSearch () =
    let tosearch = document.querySelector "#tosearch" |> getvalue
    let accuracy = document.querySelector "#accuracy" |> getvalue
    let hd = document.querySelector "#historydays" |> getvalue
    let hr = document.querySelector "#historyresults" |> getvalue
    let hb = document.querySelector "#historybookmarks" |> getselectvalue |> int
    let sm = document.querySelector "#searchmethod" |> getselectvalue |> int

    let d = {ToSearch=tosearch;Accuracy=accuracy;HistoryDays=hd;HistoryResults=hr;HistoryBookmarks=hb;SearchMethod=sm;}
    d |> StartSearch |> box |> browser.runtime.sendMessage |> Promise.start

window.onload <- (fun _ ->
    let locale:Element array = !![||]?slice?call(document.querySelectorAll ".locale")
    locale |> Array.iter (fun x ->
        let classes = x.className.Split [|' '|]
        let msg = classes |> Array.find (fun x -> x.StartsWith "lc") |> (fun x -> x.Substring 2) |> browser.i18n.getMessage
        match classes |> Array.exists (fun x -> x="lcplaceholder") with
            | true -> x.setAttribute ("placeholder",msg)
            | false -> x?innerText <- msg
    )

    window.onkeypress <- (fun x -> if x.keyCode = 13.0 then InitSearch())

    let searchbutton = (document.querySelector "#search") :?> HTMLButtonElement

    GetState |> box |> browser.runtime.sendMessage |> Promise.start

    searchbutton.onclick <- (ignore >> InitSearch)
)

let SetStatus x =
    (document.querySelector "#status").innerHTML <- x

let HandleState x =
    let searchbutton = (document.querySelector "#search") :?> HTMLButtonElement

    match x with
        | Searching _ -> searchbutton.disabled <- true | _ -> searchbutton.disabled <- false

    let getpercent x total =
        ((float x/float total)*100.0) |> int

    let renderprogress name {Done=i; Total=t} =
        sprintf "%s... <span title=\"%s...\" class=\"progress uk-text-bold\">%i%%</span>" name name (getpercent i t)

    match x with
        | Searching x ->
            match x with
                | RetrievingUrls ->
                    "Retrieving urls... <div title=\"Retrieving urls...\" uk-spinner=\"ratio: 0.5\"></div>"
                | GettingText prog ->
                    renderprogress "Getting text" prog
                | Indexing prog ->
                    renderprogress "Indexing" prog
                | SearchStage.Searching -> "Searching... <div title=\"Searching...\" uk-spinner=\"ratio: 0.5\"></div>"
            |> SetStatus
        | Finished (Pass x) when Array.length x > 0 ->
            SetStatus (x |> Array.mapi (fun i x ->
                            let pre = match x with | Bookmark _ -> browser.i18n.getMessage "bookmark" | History _ -> browser.i18n.getMessage "history"
                            let x2 = EUrlStr x |> function | x when x.Length > 65 -> sprintf "%s..." (x.Substring (0,62)) | x -> x
                            sprintf "<div class=\"uk-grid uk-margin-small\" ><span class=\"urltype uk-label uk-width-small\" >%s</span><a class=\"link uk-width-medium\" id='a%i' >%s</a></p>" pre i x2) |> Array.reduce (+))
            x |> Array.iteri (fun i x -> let a:HTMLLinkElement = (!!document.querySelector (sprintf "#a%i" i))
                                         a.onclick <-
                                            (fun _ -> browser.tabs.create (createObj ["url" ==> EUrlStr x])))
        | Finished (Pass x) ->
            SetStatus "No results found!"
        | Finished (Fail (x::_)) ->
            SetStatus x
        | _ -> ()

let HandleMessage = function
    | StateUpdate x -> HandleState x
    | _ -> ()

let f = Func<obj,unit>(fun x -> HandleMessage (unbox<Message> x))
browser.runtime.onMessage.addListener f