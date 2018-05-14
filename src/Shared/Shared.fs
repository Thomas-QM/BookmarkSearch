module Shared

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Import

open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.PowerPack.Date

open System
open System.Text.RegularExpressions

open Chessie.ErrorHandling

type JSEvent = {addListener:Func<obj,unit> -> unit; removeListener:Func<obj,unit> -> unit; hasListener: Func<obj,unit> -> bool}

type ElemUrl = History of string | Bookmark of string
let EUrlStr = function | Bookmark x | History x -> x

type State =
    | Idle
    | Searching
    | Finished of Result<ElemUrl array,string>


type HTMLDataElements = {ToSearch:string; Accuracy:string; MaxResults:string; HistoryDays:string; HistoryResults:string; HistoryBookmarks:int; SearchMethod:int;}

type [<Pojo>] BookmarkTree = {url:string option;children:BookmarkTree array option;}
type [<Pojo>] HistoryItem = {id:string; url:string option}
<<<<<<< HEAD
type WebExtBookmarks = {getTree:(BookmarkTree array -> unit) -> unit}
=======
type WebExtBookmarks = {search:obj -> JS.Promise<BookmarkTree array>}
>>>>>>> master
type WebExtHistory = {search:obj -> JS.Promise<HistoryItem array>}
type BGPage = {state:State}
type WebExtRuntime = {getBackgroundPage: unit -> JS.Promise<BGPage>; sendMessage:obj -> unit; onMessage:JSEvent}
type WebExtBrowser = {bookmarks:WebExtBookmarks; runtime:WebExtRuntime; history:WebExtHistory}

type Message =
    | StateUpdate of State
    | GetState
    | StartSearch of HTMLDataElements

[<Emit("chrome")>]
let browser:WebExtBrowser = jsNative