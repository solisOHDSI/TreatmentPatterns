SELECT
  *
FROM
  @resultSchema.@cohortTableName
WHERE
  cohort_definition_id
IN
  (@cohortIds);
