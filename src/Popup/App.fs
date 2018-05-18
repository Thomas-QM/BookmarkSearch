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
let getchecked (x:obj) =
    (x :?> HTMLInputElement).``checked``
let getselectvalue (x:obj) =
    (x :?> HTMLSelectElement).value

let InitSearch () =
    let tosearch = document.querySelector "#tosearch" |> getvalue
    let accuracy = document.querySelector "#accuracy" |> getvalue
    let hdays = document.querySelector "#historydays" |> getvalue
    let group = document.querySelector "#group" |> getchecked
    let hr = document.querySelector "#historyresults" |> getvalue
    let hb = document.querySelector "#historybookmarks" |> getselectvalue |> int
    let sm = document.querySelector "#searchmethod" |> getselectvalue |> int

    let d = {ToSearch=tosearch;Group=group;Accuracy=accuracy;HistoryDays=hdays;HistoryResults=hr;HistoryBookmarks=hb;SearchMethod=sm;}
    d |> StartSearch |> box |> browser.runtime.sendMessage

let rerender () = GetState |> box |> browser.runtime.sendMessage

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
    (document.querySelector "#group" :?> HTMLInputElement).onchange <- (fun _ -> rerender ())

    let searchbutton = (document.querySelector "#search") :?> HTMLButtonElement
    rerender()

    searchbutton.onclick <- (ignore >> InitSearch)
)

let SetStatus x =
    (document.querySelector "#status").innerHTML <- x

let renderres group x =
    match x with
        | [||] -> SetStatus "No results found!"
        | _ ->
            let divcls = "uk-grid uk-width uk-margin-remove-top uk-margin-small-bottom"
            let renderurltype =
                sprintf "<span class=\"urltype uk-label uk-width-1-3 %s\" >%s</span>"
            let bhtostr = function | Bookmark _ -> "bookmark" | History _ -> "history"
            let html =
                if not group then
                    x |> Array.mapi (fun i x ->
                            let x2 = EUrlStr x |> function | x when x.Length > 65 -> sprintf "%s..." (x.Substring (0,62)) | x -> x
                            sprintf "<div class=\"%s\" >%s<a class=\"link uk-width-2-3\" id='a%i' >%s</a></div>"
                                divcls (renderurltype (x |> bhtostr) (x |> bhtostr |> browser.i18n.getMessage)) i x2) |> Array.reduce (+)
                else
                    let rendersection offset name = function
                        | [||] -> ""
                        | elems ->
                            let elems = elems |> Array.mapi (fun i x -> sprintf "<a class=\"link uk-width\" id='a%i' >%s</a>" (i+offset) x) |> Array.reduce (+)
                            sprintf "<div class=\"%s\" >%s<div class=\"uk-width-2-3 uk-grid\" >%s</div></div>"
                                divcls (renderurltype name (name |> browser.i18n.getMessage)) elems

                    let bookmarks = x |> Array.choose (function Bookmark x -> Some x | _ -> None)
                    let history = x |> Array.choose (function History x -> Some x | _ -> None)

                    rendersection 0 "bookmarksgroup" bookmarks + rendersection (Array.length bookmarks) "history" history

            SetStatus html
            x |> Array.iteri (fun i x -> let a:HTMLLinkElement = (!!document.querySelector (sprintf "#a%i" i))
                                         a.onclick <-
                                            (fun _ -> browser.tabs.create (createObj ["url" ==> EUrlStr x])))

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
        | Finished (Pass x) ->
            let group = document.querySelector "#group" |> getchecked
            renderres group x
        | Finished (Fail (x::_)) ->
            SetStatus x
        | _ -> ()

let HandleMessage = function
    | StateUpdate x -> HandleState x
    | _ -> ()

let f = Func<obj,unit>(fun x -> HandleMessage (unbox<Message> x))
browser.runtime.onMessage.addListener f