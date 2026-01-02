module AstroCoach.Core.ConstellationCoachData

type DifficultyLevel =
| Easy
| Medium
| Hard

type ConstellationHints  =
| NoHints
| Lines
| Area
| LinesAndArea

type ConstellationCoachData = {
    constellation: SkyData.ConstellationInfo
    hints: ConstellationHints
    magnitude: float
}

type Question = {
    coach: ConstellationCoachData
    wrongConstellations: SkyData.ConstellationInfo list
}