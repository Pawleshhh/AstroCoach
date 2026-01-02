module AstroCoach.Core.Test.ConstellationCoachTest

open System
open Xunit
open AstroCoach.Core.CoachData
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