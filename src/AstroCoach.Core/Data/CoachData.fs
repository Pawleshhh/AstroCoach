module AstroCoach.Core.CoachData

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