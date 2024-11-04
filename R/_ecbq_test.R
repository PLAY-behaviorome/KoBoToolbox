e <- ecbq_complete |>
    dplyr::select(-child_sex, -age_group) |>
    dplyr::rename_with(drop_rothbart_lbl)

e_names <- names(e)

make_ecbq_lvls_numeric(e$unfamiliarperson)

e1 <- e |>
  dplyr::mutate(unfamiliarperson = make_ecbq_lvls_numeric(unfamiliarperson),
                troubletask = make_ecbq_lvls_numeric(troubletask),
                companyofchild = make_ecbq_lvls_numeric(companyofchild),
                choiceactivities = make_ecbq_lvls_numeric(choiceactivities),
                quietlysung = make_ecbq_lvls_numeric(quietlysung),
                playingoutdoors = make_ecbq_lvls_numeric(playingoutdoors),
                morethan10 = make_ecbq_lvls_numeric(morethan10),
                respondingremarks = make_ecbq_lvls_numeric(respondingremarks),
                excitedlovedadults = make_ecbq_lvls_numeric(excitedlovedadults),
                fiddlehair = make_ecbq_lvls_numeric(fiddlehair),
                roughrowdy = make_ecbq_lvls_numeric(roughrowdy),
                rockedhugged = make_ecbq_lvls_numeric(rockedhugged),
                involvednewactivity = make_ecbq_lvls_numeric(involvednewactivity),
                tirequickly = make_ecbq_lvls_numeric(tirequickly),
                callattention = make_ecbq_lvls_numeric(callattention),
                tags = make_ecbq_lvls_numeric(tags),
                noisyenvironment = make_ecbq_lvls_numeric(noisyenvironment),
                energy = make_ecbq_lvls_numeric(energy),
                vehicles = make_ecbq_lvls_numeric(vehicles),
                active = make_ecbq_lvls_numeric(active),
                forbidden = make_ecbq_lvls_numeric(forbidden),
                sadlytearful = make_ecbq_lvls_numeric(sadlytearful),
                downblue = make_ecbq_lvls_numeric(downblue),
                runhouse = make_ecbq_lvls_numeric(runhouse),
                excitingevent = make_ecbq_lvls_numeric(excitingevent),
                tempertantrum = make_ecbq_lvls_numeric(tempertantrum),
                waitpatiently = make_ecbq_lvls_numeric(waitpatiently),
                rockedsmile = make_ecbq_lvls_numeric(rockedsmile),
                mold = make_ecbq_lvls_numeric(mold),
                interactadult = make_ecbq_lvls_numeric(interactadult),
                careful = make_ecbq_lvls_numeric(careful),
                enternewplace = make_ecbq_lvls_numeric(enternewplace),
                crymorethan3 = make_ecbq_lvls_numeric(crymorethan3),
                easilysoothed = make_ecbq_lvls_numeric(easilysoothed),
                busyother = make_ecbq_lvls_numeric(busyother),
                differentpeople = make_ecbq_lvls_numeric(differentpeople)
                )

e1_cor <- cor(e1, use="pairwise.complete.obs")