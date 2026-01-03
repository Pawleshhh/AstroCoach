module AstroCoach.Core.ConstellationCoach

open SkyData
open ConstellationCoachData

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

let createConstellationCoach
    (constellation: ConstellationInfo)
    (difficulty: DifficultyLevel)
    : ConstellationCoachData =
    
    let hints = difficultyHints difficulty
    let maxMag = difficultyMaxMagnitude difficulty

    {
        constellation = constellation
        hints = hints
        magnitude = maxMag
    }

let generateCoachWithDifficulty
    (rng: Rng)
    (getConstellation: GetConstellation)
    (difficulty: DifficultyLevel)
    : ConstellationCoachData =
    
    let constellation = randomConstellation rng getConstellation

    createConstellationCoach constellation difficulty

/// Sample up to `count` distinct wrong constellation indices using the provided RNG,
/// excluding `excludeIndex`. Uses a partial Fisher–Yates shuffle to avoid retrying RNG
/// until a non-excluded value is produced.
let private sampleWrongConstellations
    (rng: Rng)
    (getConstellation: GetConstellation)
    (excludeIndex: int)
    (count: int)
    : ConstellationInfo list =

    if count <= 0 then
        []
    else
        // Build candidate indices excluding the correct one
        let candidates =
            Array.init constellationCount id
            |> Array.filter (fun i -> i <> excludeIndex)

        let n = Array.length candidates
        let k = if count > n then n else count

        // Partial Fisher–Yates: produce first k shuffled elements deterministically using rng
        for i = 0 to k - 1 do
            // pick j in [i .. n-1]
            let j = i + randomNum rng (n - i)
            let tmp = candidates.[i]
            candidates.[i] <- candidates.[j]
            candidates.[j] <- tmp

        candidates.[0 .. k - 1]
        |> Array.toList
        |> List.map (fun idx -> getConstellation idx)

let createQuestion
    (coach: ConstellationCoachData)
    (makeQuestion: ConstellationCoachData -> ConstellationQuestion) =

    makeQuestion coach

let createClosedQuestion
    (wrongConstellations: ConstellationInfo list)
    (coach: ConstellationCoachData) =

    ClosedQuestion({ coach = coach }, { wrongConstellations = wrongConstellations })

let createOpenQuestion (coach: ConstellationCoachData) =
    OpenQuestion({ coach = coach })

let generateClosedQuestion
    (rng: Rng)
    (getConstellation: GetConstellation)
    wrongCount
    : ConstellationQuestion =

    let coach = generateCoach rng getConstellation
    let correctId = coach.constellation.id

    let excludeIndex = int correctId

    let wrongConstellations =
        sampleWrongConstellations rng getConstellation excludeIndex wrongCount

    createQuestion coach (createClosedQuestion wrongConstellations)

let generateOpenQuestion
    (rng: Rng)
    (getConstellation: GetConstellation)
    : ConstellationQuestion =

    let coach = generateCoach rng getConstellation
    createQuestion coach createOpenQuestion

let private difficultyWrongCount difficulty =
    match difficulty with
    | Easy -> 1
    | Medium -> 2
    | Hard -> 3

let generateClosedQuestionWithDifficulty
    (rng: Rng)
    (getConstellation: GetConstellation)
    (difficulty: DifficultyLevel)
    : ConstellationQuestion =

    let coach = generateCoachWithDifficulty rng getConstellation difficulty
    let correctId = coach.constellation.id
    
    let excludeIndex = int correctId

    let wrongConstellations =
        (difficultyWrongCount difficulty)
        |> sampleWrongConstellations rng getConstellation excludeIndex
        
    createQuestion coach (createClosedQuestion wrongConstellations)

let generateOpenQuestionWithDifficulty
    (rng: Rng)
    (getConstellation: GetConstellation)
    (difficulty: DifficultyLevel)
    : ConstellationQuestion =

    let coach = generateCoachWithDifficulty rng getConstellation difficulty
    
    createQuestion coach createOpenQuestion