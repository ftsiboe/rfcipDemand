
<div style="font-size: 0.75em; line-height: 1.25;">

⚠️ <strong>Disclaimer:</strong> These data are provided <strong>“as
is”</strong>, without warranty of any kind. The authors make no
guarantees regarding accuracy or suitability for any purpose. Use of
these data and any results derived from them is entirely at the user’s
own risk. The contents of this release reflect independent research and
do not represent the official positions or policies of the USDA, the
Risk Management Agency (RMA), the Federal Crop Insurance Corporation
(FCIC), or any Approved Insurance Provider.

</div>

Database was last updated on 2026-01-30

------------------------------------------------------------------------

The release provides replication code and output for generating
instruments for the United States Crop Insurance Program as described in

- [Econometric identification of crop insurance
  participation](https://doi.org/10.1017/age.2023.13)

- [The crop insurance demand response to premium subsidies: Evidence
  from US Agriculture](https://doi.org/10.1016/j.foodpol.2023.102505)

- [Effects of crop insurance premium subsidies on crop
  acreage](https://doi.org/10.1093/ajae/aax058)

#### Accessing the estimated instrumental variables fcip demand in R

- Project package: `rfcipDemand::fcip_demand_instruments`
- Generate from raw data: `generate_fcip_instruments.R`

#### Files in this directory

| File | Description |
|:---|:---|
| `2023 TsiboeTurner Econometric-identification-of-crop-insurance-participation.pdf` | Published article for the proposed instrument ([doi:10.1017/age.2023.13](https://doi.org/10.1017/age.2023.13)) |
| `exogeneity_justification.docx` | Exogeneity argument for the proposed instruments |
| `2009 FCIC Rate Methodology Handbook APH.pdf` | 2009 FCIC Rate Methodology Handbook APH published by RMA ([legacy PDF](https://legacy.rma.usda.gov/pubs/2008/ratemethodology.pdf)) |
| `fcip_instruments_formulation.R` | Script to formulate the instrument at the county–crop level on a historical basis |
| `fcip_demand_instruments.rds` | Output file |

#### Definitions for each column in the output file (`fcip_demand_instruments.rds`)

| Column | Definition |
|:---|:---|
| `commodity_year` | The identifier that represents the year in which the crop/commodity is normally harvested and indicates the policy year for which coverage was provided. |
| `state_abbreviation` | USPS state abbreviation. |
| `state_code` | The FIPS code that denotes the State in which the insured farm is located. |
| `county_name` | Name of the county. |
| `county_code` | The FIPS code that denotes the County in which the insured farm is located. |
| `commodity_name` | Name of the crop/commodity. |
| `commodity_code` | The Risk Management Agency (RMA) code that denotes the crop/commodity for which the policy is issued. |
| `tau_sob` | An approximation of the “target rate” as described by Tsiboe & Turner (2023). Tracks changes in the cost of crop insurance that are plausibly exogenous to any one producer; estimated from historic SOB data. |
| `tau_sob` | An approximation of the “target rate” as described by Tsiboe & Turner (2023). Tracks changes in the cost of crop insurance that are plausibly exogenous to any one producer; estimated by aggregating directly from RMA’s Actuarial Data Master. |
| `tau_final` | Same as `tau_adm`, with any missing values filled in using `tau_sob`. |
| `subsidy_rate_65` | National subsidy rate for 65% coverage‐level individual crop insurance policies. Constructed as described by Yu et al. (2018) (see footnote on Figure 1). Can be used as an instrument on its own or interacted with `tau` for greater temporal variation. |
| `subsidy_rate_75` | National subsidy rate for 75% coverage‐level individual crop insurance policies. Constructed as described by Yu et al. (2018) (see footnote on Figure 1). Can be used as an instrument on its own or interacted with `tau` for greater temporal variation. |

------------------------------------------------------------------------

If you find this repository useful, please star this project and cite
our papers listed above.

⭐ *If you find this data release useful, please consider starring the
repository.*
