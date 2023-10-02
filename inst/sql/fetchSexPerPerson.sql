SELECT
  person_id,
  concept_name AS sex
FROM
  @cdmSchema.person
INNER JOIN
  @cdmSchema.concept
ON
  person.gender_concept_id = concept.concept_id
WHERE
  person_id
IN
  (@personIds);
