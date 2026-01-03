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

type QuestionBaseInfo = {
    coach: ConstellationCoachData
}

type ClosedQuestionInfo = {
    wrongConstellations: SkyData.ConstellationInfo list
}

type ConstellationQuestion =
| OpenQuestion of QuestionBaseInfo
| ClosedQuestion of QuestionBaseInfo * ClosedQuestionInfo