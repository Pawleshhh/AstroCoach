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
        createCoachWithDifficulty rng fakeStorage DifficultyLevel.Easy

    Assert.Equal(ConstellationHints.LinesAndArea, coach.hints)
    Assert.Equal(4.0, coach.magnitude)

[<Fact>]
let ``Medium difficulty gives area hints and magnitude 6`` () =
    let rng = rngFromList [ 20 ]

    let coach =
        createCoachWithDifficulty rng fakeStorage DifficultyLevel.Medium

    Assert.Equal(ConstellationHints.Area, coach.hints)
    Assert.Equal(6.0, coach.magnitude)

[<Fact>]
let ``Hard difficulty gives no hints and magnitude 10`` () =
    let rng = rngFromList [ 5 ]

    let coach =
        createCoachWithDifficulty rng fakeStorage DifficultyLevel.Hard

    Assert.Equal(ConstellationHints.NoHints, coach.hints)
    Assert.Equal(10.0, coach.magnitude)

[<Fact>]
let ``createCoachWithDifficulty selects constellation via RNG`` () =
    let rng = rngFromList [ 7 ]

    let coach =
        createCoachWithDifficulty rng fakeStorage DifficultyLevel.Easy

    Assert.Equal(7L, coach.constellation.id)


[<Fact>]
let ``generateQuestion delegates to generateCoach for coach selection`` () =
    let seq = [ 0; 1; 42; 0; 0; 0 ]
    let rng1 = rngFromList seq
    let rng2 = rngFromList seq

    let expectedCoach = generateCoach rng1 fakeStorage
    let question = generateQuestion rng2 fakeStorage 3

    Assert.Equal(expectedCoach.constellation.id, question.coach.constellation.id)

[<Fact>]
let ``generateQuestion returns the requested number of distinct wrong constellations excluding the correct one`` () =
    let rng = rngFromList [ 0; 0; 10; 0; 1; 2 ]
    let question = generateQuestion rng fakeStorage 3

    let wrongIds = question.wrongConstellations |> List.map (fun c -> c.id)
    Assert.Equal(3, List.length wrongIds)
    Assert.DoesNotContain(question.coach.constellation.id, wrongIds)
    Assert.Equal(List.length wrongIds, (wrongIds |> Set.ofList |> Set.count))

[<Fact>]
let ``generateQuestion caps wrongCount to available candidates (constellationCount - 1)`` () =
    let manyZeros = List.replicate 90 0
    let rng = rngFromList manyZeros
    let question = generateQuestion rng fakeStorage 200

    Assert.Equal(88 - 1, List.length question.wrongConstellations)
    Assert.DoesNotContain(question.coach.constellation.id, question.wrongConstellations |> List.map (fun c -> c.id))

[<Fact>]
let ``createQuestionWithDifficulty delegates to createCoachWithDifficulty for coach selection`` () =
    // same RNG sequence used for both to ensure identical coach outcome
    let seq = [ 7; 1; 2; 3 ]
    let rng1 = rngFromList seq
    let rng2 = rngFromList seq

    let expectedCoach = createCoachWithDifficulty rng1 fakeStorage DifficultyLevel.Medium
    let question = createQuestionWithDifficulty rng2 fakeStorage DifficultyLevel.Medium

    Assert.Equal(expectedCoach.constellation.id, question.coach.constellation.id)

[<Fact>]
let ``createQuestionWithDifficulty returns one wrong for Easy, excludes correct and ensures uniqueness`` () =
    // first value picks correct constellation; second value is consumed by sampling loop (k = 1)
    let rng = rngFromList [ 10; 0 ]
    let question = createQuestionWithDifficulty rng fakeStorage DifficultyLevel.Easy

    Assert.Equal(1, List.length question.wrongConstellations)
    Assert.DoesNotContain(question.coach.constellation.id, question.wrongConstellations |> List.map (fun c -> c.id))
    Assert.Equal(1, (question.wrongConstellations |> List.map (fun c -> c.id) |> Set.ofList |> Set.count))

[<Fact>]
let ``createQuestionWithDifficulty returns two wrongs for Medium, excludes correct and ensures uniqueness`` () =
    // first value picks correct constellation; next two values are consumed by sampling loop (k = 2)
    let rng = rngFromList [ 5; 1; 2 ]
    let question = createQuestionWithDifficulty rng fakeStorage DifficultyLevel.Medium

    Assert.Equal(2, List.length question.wrongConstellations)
    Assert.DoesNotContain(question.coach.constellation.id, question.wrongConstellations |> List.map (fun c -> c.id))
    Assert.Equal(2, (question.wrongConstellations |> List.map (fun c -> c.id) |> Set.ofList |> Set.count))

[<Fact>]
let ``createQuestionWithDifficulty returns three wrongs for Hard, excludes correct and ensures uniqueness`` () =
    // first value picks correct constellation; next three values are consumed by sampling loop (k = 3)
    let rng = rngFromList [ 7; 3; 4; 5 ]
    let question = createQuestionWithDifficulty rng fakeStorage DifficultyLevel.Hard

    Assert.Equal(3, List.length question.wrongConstellations)
    Assert.DoesNotContain(question.coach.constellation.id, question.wrongConstellations |> List.map (fun c -> c.id))
    Assert.Equal(3, (question.wrongConstellations |> List.map (fun c -> c.id) |> Set.ofList |> Set.count))