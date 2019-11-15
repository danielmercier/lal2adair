open Libadalang

let ctx = AnalysisContext.create ()

let u = AnalysisContext.get_from_file ctx "a.adb"
