module AstroCoach.Core.SkyData

type BaseSkyObject = {
    id: int64
    ra: float
    dec: float
    time: System.DateTime
}

type StarInfo = {
    magnitude: float
    spectralType: string
}

type SkyObject =
| Star of BaseSkyObject * StarInfo

type ConstellationInfo = {
    id: int64
    shortName: string
    fullName: string
    stars: SkyObject list
}
