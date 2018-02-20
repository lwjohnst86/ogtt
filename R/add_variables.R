
homair <- function(fpg, fi) {
    (fpg * fi) / 22.5
}

#' @describeIn add_variables Add insulin resistance/sensitivity variables (HOMA,
#'   ISI) to the dataset.
isi_matsuda <- function(glucose, insulin) {
    assertive::assert_are_same_length(glucose, insulin)
    n_samples <- length(glucose)

    # mean_glucose <- rowSums()
    # data %>%
    #     dplyr::mutate(
    #         MeanGlucose = (Glucose0 + Glucose30 + Glucose120) / 3,
    #         MeanInsulin = (Insulin0 + Insulin30 + Insulin120) / 3,
    #         ISI = 10000 / sqrt((Glucose0 * Insulin0) * (MeanGlucose * MeanInsulin)),
    #         MeanGlucose_2ogtt = (Glucose0 + Glucose120) / 2,
    #         MeanInsulin_2ogtt = (Insulin0 + Insulin120) / 2,
    #         ISI_2ogtt = 10000 / sqrt((Glucose0 * Insulin0) * (MeanGlucose_2ogtt * MeanInsulin_2ogtt))
    #     ) %>%
    #     dplyr::select(-MeanGlucose,-MeanInsulin,-MeanGlucose_2ogtt,-MeanInsulin_2ogtt)
}

igi <- function() {

}

#' @describeIn add_variables Add beta-cell function variables (ISSI-2 and IGI/IR) to
#'   the dataset.
add_outcomes_bcf <- function(data, force_dict_update = FALSE) {

    data %>%
        dplyr::mutate(
            GlucoseDiff = Glucose30 - Glucose0,
            InsulinDiff = Insulin30 - Insulin0,
            IGIIR = (abs(InsulinDiff) / abs(GlucoseDiff + 1)) / HOMA,
            GlucoseAUC = (((Glucose0 + Glucose30) / 2) * (0.5 - 0)) +
                (((
                    Glucose30 + Glucose120
                ) / 2) * (2 - 0.5)),
            InsulinAUC = (((Insulin0 + Insulin30) / 2) * (0.5 - 0)) +
                (((
                    Insulin30 + Insulin120
                ) / 2) * (2.0 - 0.5)),
            ISSI2 = (InsulinAUC / GlucoseAUC) * ISI,
            ## I've calculated ISSI-2 with also using only 2 OGTT points.
            GlucoseAUC_2ogtt = ((Glucose0 + Glucose120) / 2) * (2 - 0),
            InsulinAUC_2ogtt = ((Insulin0 + Insulin120) / 2) * (2 - 0),
            ISSI2_2ogtt = (InsulinAUC_2ogtt / GlucoseAUC_2ogtt) * ISI_2ogtt
        ) %>%
        dplyr::select(-GlucoseDiff,-InsulinDiff)
}

#' @describeIn add_variables Add insulin clearance and C-peptide AUC variables to
#' the dataset.
add_outcomes_ic <- function(data, force_dict_update = FALSE) {

    unit <- 'NA'
    type <- 'Numeric'
    ref <- 'see view_methods_manual().'

    c(dict_entry(
        'CPeptideAUC', unit, type,
        paste('C-Peptide area under the curve;', ref)
    ), dict_entry(
        'InsulinClearance', unit, type,
        paste('(Hepatic) insulin clearance;', ref)
    )) %>%
        dict_update(force.update = force_dict_update)

    data %>%
        dplyr::mutate(
            CPeptideAUC = (((CPeptide0 + CPeptide30) / 2) * (0.5 - 0)) +
                (((CPeptide30 + CPeptide120) / 2) * (2 - 0.5)),
            InsulinClearance = CPeptideAUC / InsulinAUC
        )
}

#' @describeIn add_variables Add total fatty acid concentrations from either normal
#' or trans measurements to the dataset.
add_total_fa <- function(data, force_dict_update = FALSE) {

    unit <- 'nmol/mL'
    type <- 'Numeric'
    ref.no.tfa <- '(not including the trans fatty acid data).'
    ref.tfa <- '(including the trans fatty acid data).'
    ne <- 'Total non-esterified fatty acids'
    ce <- 'Total cholesteryl ester fatty acids'
    pl <- 'Total phospholipid fatty acids'
    tg <- 'Total triacylglycerol fatty acids'

    c(dict_entry(
        'TotalNE', unit, type,
        paste(ne, ref.no.tfa)
    ), dict_entry(
        'TotalCE', unit, type,
        paste(ce, ref.no.tfa)
    ), dict_entry(
        'TotalPL', unit, type,
        paste(pl, ref.no.tfa)
    ), dict_entry(
        'TotalTG', unit, type,
        paste(tg, ref.no.tfa)
    ), dict_entry(
        'TransfaTotalNE', unit, type,
        paste(ne, ref.tfa)
    ), dict_entry(
        'TransfaTotalCE', unit, type,
        paste(ce, ref.tfa)
    ), dict_entry(
        'TransfaTotalPL', unit, type,
        paste(pl, ref.tfa)
    ), dict_entry(
        'TransfaTotalTG', unit, type,
        paste(tg, ref.tfa)
    )) %>%
        dict_update(force.update = force_dict_update)

    data %>%
        dplyr::mutate(
            TotalNE = .calculate_row_sums('^ne', data),
            TotalCE = .calculate_row_sums('^ce', data),
            TotalPL = .calculate_row_sums('^pl', data),
            TotalTG = .calculate_row_sums('^tg', data),
            TransfaTotalNE = .calculate_row_sums('^transfa_ne', data),
            TransfaTotalCE = .calculate_row_sums('^transfa_ce', data),
            TransfaTotalPL = .calculate_row_sums('^transfa_pl', data),
            TransfaTotalTG = .calculate_row_sums('^transfa_tg', data)
        )
}

#' @describeIn add_variables Add the metabolic syndrome (binary and components) to
#'   the dataset. (incomplete)
add_metabolic_syndrome <- function(data) {

}

#' @describeIn add_variables Combine parent ethnicity to determine participant
#' ethnicity. When parents ethnicities did not match, the participant was
#' classified as 'Other'.
add_ethnicity <- function(data) {
    add_single_variable(data, function(x) {
        x %>%
        dplyr::mutate(Ethnicity = factor(ifelse(
            EthnFather == EthnMother, as.character(EthnMother), 'Other'
        )))
        },
        'Ethnicity', 'NA', 'Discrete',
        'Ethnicity based on parents ethnicity.')
}


#' @describeIn add_variables Add BMI to the prefinal dataset.
add_bmi <- function(data) {
    add_single_variable(data, function(x) {
        x %>%
            dplyr::mutate(BMI = Weight / ((Height/100)^2))
        },
        name = 'BMI', unit = 'kg/m^2', type = 'Numeric',
        description = 'Body Mass Index')
}

#' @describeIn add_variables Add mean arterial pressure to the prefinal dataset.
add_map <- function(data) {
    add_single_variable(data, function(x) {
        x %>%
            dplyr::mutate(MeanArtPressure = ((Systolic - Diastolic)/3) + Diastolic)
    },
    'MeanArtPressure', 'mmHg', 'Numeric',
    'Mean arterial pressure; defines the average blood pressure of an individual.')
}
