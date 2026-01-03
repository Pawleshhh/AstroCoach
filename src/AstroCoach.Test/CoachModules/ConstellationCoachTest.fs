module AstroCoach.Core.Test.ConstellationCoachTest

open System
open Xunit
open AstroCoach.Core.ConstellationCoachData
open AstroCoach.Core.SkyData
open AstroCoach.Core.ConstellationCoach

let rngFromList values =
    let state = ref values
    fun () ->
        match !state with
        | x :: xs ->
            state := xs
            x
        | [] ->
            failwith "RNG exhausted"

let mkStar id mag =
    SkyObject.Star(
        { id = id
          ra = 0.0
          dec = 0.0
          time = DateTime.UnixEpoch },
        { magnitude = mag
          spectralType = "G" }
    )

let fakeConstellations =
    [| for i in 0 .. 87 ->
        { id = int64 i
          shortName = $"C{i}"
          fullName = $"Constellation {i}"
          stars = [ mkStar (int64 i) 1.0 ] } |]

let fakeStorage index =
    fakeConstellations.[index]

// Helpers to work with the ConstellationQuestion union
let private coachOfQuestion q =
    match q with
    | OpenQuestion info -> info.coach
    | ClosedQuestion (info, _) -> info.coach

let private wrongsOfQuestion q =
    match q with
    | ClosedQuestion (_, info) -> info.wrongConstellations
    | OpenQuestion _ -> []

[<Fact>]
let ``generateCoach produces magnitude between -2 and 10`` () =
    let rng = rngFromList [ 0; 0; 0; 0 ]

    let coach = generateCoach rng fakeStorage

    Assert.InRange(coach.magnitude, -2.0, 10.0)

[<Theory>]
[<InlineData(0, 0)>]
[<InlineData(1, 1)>]
[<InlineData(2, 2)>]
[<InlineData(3, 3)>]
[<InlineData(7, 3)>]
let ``generateCoach maps RNG to hints correctly`` (rngValue: int, hintCode: int) =
    let expectedHint =
        match hintCode with
        | 0 -> ConstellationHints.NoHints
        | 1 -> ConstellationHints.Lines
        | 2 -> ConstellationHints.Area
        | _ -> ConstellationHints.LinesAndArea

    let rng = rngFromList [ 0; rngValue; 0; 0 ]

    let coach = generateCoach rng fakeStorage

    Assert.Equal(expectedHint, coach.hints)

[<Fact>]
let ``generateCoach selects expected constellation`` () =
    let rng = rngFromList [ 0; 0; 42; 0 ]

    let coach = generateCoach rng fakeStorage

    Assert.Equal(42L, coach.constellation.id)

[<Fact>]
let ``Easy difficulty gives full hints and magnitude 4`` () =
    let rng = rngFromList [ 10 ]

    let coach =
        generateCoachWithDifficulty rng fakeStorage DifficultyLevel.Easy

    Assert.Equal(ConstellationHints.LinesAndArea, coach.hints)
    Assert.Equal(4.0, coach.magnitude)

[<Fact>]
let ``Medium difficulty gives area hints and magnitude 6`` () =
    let rng = rngFromList [ 20 ]

    let coach =
        generateCoachWithDifficulty rng fakeStorage DifficultyLevel.Medium

    Assert.Equal(ConstellationHints.Area, coach.hints)
    Assert.Equal(6.0, coach.magnitude)

[<Fact>]
let ``Hard difficulty gives no hints and magnitude 10`` () =
    let rng = rngFromList [ 5 ]

    let coach =
        generateCoachWithDifficulty rng fakeStorage DifficultyLevel.Hard

    Assert.Equal(ConstellationHints.NoHints, coach.hints)
    Assert.Equal(10.0, coach.magnitude)

[<Fact>]
let ``generateCoachWithDifficulty selects constellation via RNG`` () =
    let rng = rngFromList [ 7 ]

    let coach =
        generateCoachWithDifficulty rng fakeStorage DifficultyLevel.Easy

    Assert.Equal(7L, coach.constellation.id)


[<Fact>]
let ``generateClosedQuestion delegates to generateCoach for coach selection`` () =
    let seq = [ 0; 1; 42; 0; 0; 0 ]
    let rng1 = rngFromList seq
    let rng2 = rngFromList seq

    let expectedCoach = generateCoach rng1 fakeStorage
    let question = generateClosedQuestion rng2 fakeStorage 3

    Assert.Equal(expectedCoach.constellation.id, (coachOfQuestion question).constellation.id)

[<Fact>]
let ``generateClosedQuestion returns the requested number of distinct wrong constellations excluding the correct one`` () =
    let rng = rngFromList [ 0; 0; 10; 0; 1; 2 ]
    let question = generateClosedQuestion rng fakeStorage 3

    let wrongIds = (wrongsOfQuestion question) |> List.map (fun c -> c.id)
    Assert.Equal(3, List.length wrongIds)
    Assert.DoesNotContain((coachOfQuestion question).constellation.id, wrongIds)
    Assert.Equal(List.length wrongIds, (wrongIds |> Set.ofList |> Set.count))

[<Fact>]
let ``generateClosedQuestion caps wrongCount to available candidates (constellationCount - 1)`` () =
    let manyZeros = List.replicate 90 0
    let rng = rngFromList manyZeros
    let question = generateClosedQuestion rng fakeStorage 200

    let wrongIds = (wrongsOfQuestion question) |> List.map (fun c -> c.id)
    Assert.Equal(88 - 1, List.length wrongIds)
    Assert.DoesNotContain((coachOfQuestion question).constellation.id, wrongIds)

[<Fact>]
let ``generateClosedQuestionWithDifficulty delegates to generateCoachWithDifficulty  for coach selection`` () =
    // same RNG sequence used for both to ensure identical coach outcome
    let seq = [ 7; 1; 2; 3 ]
    let rng1 = rngFromList seq
    let rng2 = rngFromList seq

    let expectedCoach = generateCoachWithDifficulty rng1 fakeStorage DifficultyLevel.Medium
    let question = generateClosedQuestionWithDifficulty rng2 fakeStorage DifficultyLevel.Medium

    Assert.Equal(expectedCoach.constellation.id, (coachOfQuestion question).constellation.id)

[<Fact>]
let ``generateClosedQuestionWithDifficulty returns one wrong for Easy, excludes correct and ensures uniqueness`` () =
    // first value picks correct constellation; second value is consumed by sampling loop (k = 1)
    let rng = rngFromList [ 10; 0 ]
    let question = generateClosedQuestionWithDifficulty rng fakeStorage DifficultyLevel.Easy

    let wrongIds = (wrongsOfQuestion question) |> List.map (fun c -> c.id)
    Assert.Equal(1, List.length wrongIds)
    Assert.DoesNotContain((coachOfQuestion question).constellation.id, wrongIds)
    Assert.Equal(1, (wrongIds |> Set.ofList |> Set.count))

[<Fact>]
let ``generateClosedQuestionWithDifficulty returns two wrongs for Medium, excludes correct and ensures uniqueness`` () =
    // first value picks correct constellation; next two values are consumed by sampling loop (k = 2)
    let rng = rngFromList [ 5; 1; 2 ]
    let question = generateClosedQuestionWithDifficulty rng fakeStorage DifficultyLevel.Medium

    let wrongIds = (wrongsOfQuestion question) |> List.map (fun c -> c.id)
    Assert.Equal(2, List.length wrongIds)
    Assert.DoesNotContain((coachOfQuestion question).constellation.id, wrongIds)
    Assert.Equal(2, (wrongIds |> Set.ofList |> Set.count))

[<Fact>]
let ``generateClosedQuestionWithDifficulty returns three wrongs for Hard, excludes correct and ensures uniqueness`` () =
    // first value picks correct constellation; next three values are consumed by sampling loop (k = 3)
    let rng = rngFromList [ 7; 3; 4; 5 ]
    let question = generateClosedQuestionWithDifficulty rng fakeStorage DifficultyLevel.Hard

    let wrongIds = (wrongsOfQuestion question) |> List.map (fun c -> c.id)
    Assert.Equal(3, List.length wrongIds)
    Assert.DoesNotContain((coachOfQuestion question).constellation.id, wrongIds)
    Assert.Equal(3, (wrongIds |> Set.ofList |> Set.count))

[<Fact>]
let ``createConstellationCoach constructs coach with expected hints and magnitude`` () =
    let constellation = fakeConstellations.[4]
    let coach = createConstellationCoach constellation DifficultyLevel.Medium

    Assert.Equal(4L, coach.constellation.id)
    Assert.Equal(ConstellationHints.Area, coach.hints)
    Assert.Equal(6.0, coach.magnitude)

[<Fact>]
let ``createOpenQuestion wraps coach into OpenQuestion`` () =
    let coach = createConstellationCoach fakeConstellations.[2] DifficultyLevel.Easy
    let q = createOpenQuestion coach

    match q with
    | OpenQuestion info -> Assert.Equal(coach.constellation.id, info.coach.constellation.id)
    | _ -> failwith "Expected OpenQuestion"

[<Fact>]
let ``createClosedQuestion wraps wrong constellations and coach into ClosedQuestion`` () =
    let wrongs = [ fakeConstellations.[1]; fakeConstellations.[3] ]
    let coach = createConstellationCoach fakeConstellations.[2] DifficultyLevel.Hard
    let q = createClosedQuestion wrongs coach

    match q with
    | ClosedQuestion (info, winfo) ->
        Assert.Equal(coach.constellation.id, info.coach.constellation.id)
        let wrongIds = winfo.wrongConstellations |> List.map (fun c -> c.id)
        let expected = [| 1L; 3L |]
        Assert.Equal(expected, wrongIds)
    | _ -> failwith "Expected ClosedQuestion"

[<Fact>]
let ``createQuestion applies provided maker function to coach`` () =
    let coach = createConstellationCoach fakeConstellations.[6] DifficultyLevel.Easy
    let createdByHelper = createOpenQuestion coach
    let createdByCreate = createQuestion coach createOpenQuestion

    match createdByHelper, createdByCreate with
    | OpenQuestion h1, OpenQuestion h2 ->
        Assert.Equal(h1.coach.constellation.id, h2.coach.constellation.id)
    | _ -> failwith "Expected two OpenQuestion results"