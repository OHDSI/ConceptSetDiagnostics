SELECT @cohort_id cohort_definition_id,
        person_id subject_id,
        concept_date cohort_start_date,
        concept_date cohort_end_date
INSERT INTO @temp_cohort_table_name        
FROM @concept_id_table;