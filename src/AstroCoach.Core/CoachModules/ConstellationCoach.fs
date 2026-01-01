module AstroCoach.Core.ConstellationCoach

open SkyData
open CoachData

type Rng = unit -> int
type GetConstellation = int -> ConstellationInfo


let constellationCount = 88
let private randomNum (rng: Rng) n = abs (rng ()) % n

let private randomConstellation rng get =
    get (randomNum rng constellationCount)

let generateCoach
    (rng: Rng)
    (getConstellation: GetConstellation)
    : ConstellationCoachData =
    let minMag = -2
    let maxMag = 10
    let mag =
        float (randomNum rng (maxMag - minMag + 1) + minMag)

    let hints =
        match randomNum rng 4 with
        | 0 -> NoHints
        | 1 -> Lines
        | 2 -> Area
        | _ -> LinesAndArea

    let constellation = randomConstellation rng getConstellation

    {
        constellation = constellation
        hints = hints
        magnitude = mag
    }

let private difficultyHints difficulty =
    match difficulty with
    | Easy -> LinesAndArea
    | Medium -> Area
    | Hard -> NoHints

let private difficultyMaxMagnitude difficulty =
    match difficulty with
    | Easy -> 4
    | Medium -> 6
    | Hard -> 10

let createCoachWithDifficulty
    (rng: Rng)
    (getConstellation: GetConstellation)
    (difficulty: DifficultyLevel)
    : ConstellationCoachData =
    
    let constellation = randomConstellation rng getConstellation

    let hints = difficultyHints difficulty
    let maxMag = difficultyMaxMagnitude difficulty

    {
        constellation = constellation
        hints = hints
        magnitude = maxMag
    }