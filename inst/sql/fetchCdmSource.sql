SELECT
  cdm_source_name,
  cdm_source_abbreviation,
  cdm_release_date,
  vocabulary_version
FROM
  @cdmSchema.cdm_source;