#include "../../src/core/command_model.h"
#include "../../src/core/dataset_model.h"
#include "../../src/core/dataset_protocol.h"

#include <cassert>
#include <sstream>
#include <string>
#include <vector>

using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::DisplayValueForDataFrameCell;
using rlispstat::core::EncodeCommandField;
using rlispstat::core::FindDataColumnInDataFrame;
using rlispstat::core::ParseDataFramePayload;
using rlispstat::core::WriteDataFramePayloadForR;

int main()
{
    {
        std::vector<std::string> lines = {"OTHER"};
        std::size_t cursor = 0;
        DataFrameModel df;
        bool parsed = true;
        std::string error;
        assert(ParseDataFramePayload(lines, cursor, "cars", df, &parsed, error));
        assert(!parsed);
        assert(cursor == 0);
        assert(error.empty());
    }

    {
        std::vector<std::string> lines = {
            "DATAFRAME", "2", "2",
            "mpg", "Numérico", "21", "22",
            "transmission", "unsupported_type", "auto", "manual",
            "DATADISPLAY", "1",
            "mpg", "21.0", "22.0",
            "DATLEVELS", "1",
            "transmission", "2", "manual", "auto",
            "DATAMETA", "2",
            "mpg", EncodeCommandField("Miles|per\tgallon"), EncodeCommandField("desc\nline"), "2",
            "transmission", "", "NA", "-1"
        };
        std::size_t cursor = 0;
        DataFrameModel df;
        bool parsed = false;
        std::string error;
        assert(ParseDataFramePayload(lines, cursor, "cars", df, &parsed, error));
        assert(parsed);
        assert(cursor == lines.size());
        assert(error.empty());
        assert(df.group == "cars");
        assert(df.rows == 2);
        assert(df.columns.size() == 2);

        const DataColumn *mpg = FindDataColumnInDataFrame(df, "mpg");
        assert(mpg != nullptr);
        assert(mpg->type == "numeric");
        assert(mpg->displayName == "Miles|per\tgallon");
        assert(mpg->description == "desc\nline");
        assert(mpg->decimals == 2);
        assert(mpg->displayValues.size() == 2);
        assert(mpg->displayValues[0] == "21.0");
        assert(DisplayValueForDataFrameCell(df, *mpg, 0) == "21.0");

        const DataColumn *transmission = FindDataColumnInDataFrame(df, "transmission");
        assert(transmission != nullptr);
        assert(transmission->type == "character");
        assert(transmission->displayName == "transmission");
        assert(transmission->description.empty());
        assert((transmission->definedLevels == std::vector<std::string>{"manual", "auto"}));
    }

    {
        std::vector<std::string> lines = {
            "DATAFRAME", "2", "1",
            "age", "numeric", "10", "NA",
            "IMPUTATION_SPARSE",
            "multiple_imputation", "imp1", "source1", "2", "1", "version", "1",
            "age", "1", "2", "NA", "20", "21"
        };
        std::size_t cursor = 0;
        DataFrameModel df;
        bool parsed = false;
        std::string error;
        assert(ParseDataFramePayload(lines, cursor, "imp data", df, &parsed, error));
        assert(parsed);
        assert(cursor == lines.size());
        assert(error.empty());
        assert(df.datasetType == "multiple_imputation");
        assert(df.imputationId == "imp1");
        assert(df.sourceDatasetId == "source1");
        assert(df.imputationCount == 2);
        assert(df.activeImputationVersion == 1);

        const DataColumn *age = FindDataColumnInDataFrame(df, "age");
        assert(age != nullptr);
        assert(age->imputedMissing.size() == 2);
        assert(!age->imputedMissing[0]);
        assert(age->imputedMissing[1]);
        assert(age->imputationOriginalSparse.find(1)->second == "NA");
        assert(age->imputationValuesSparse.size() == 2);
        assert(age->imputationValuesSparse[0].find(1)->second == "20");
        assert(age->imputationValuesSparse[1].find(1)->second == "21");
        assert(age->values[1] == "20");
        assert(DisplayValueForDataFrameCell(df, *age, 1) == "20");
    }

    {
        std::vector<std::string> lines = {
            "DATAFRAME", "2", "1",
            "score", "numeric", "NA", "5",
            "IMPUTATION",
            "multiple_imputation", "imp2", "source2", "2", "2", "version", "1",
            "score",
            "1", "0",
            "NA", "5",
            "10", "5",
            "11", "5"
        };
        std::size_t cursor = 0;
        DataFrameModel df;
        bool parsed = false;
        std::string error;
        assert(ParseDataFramePayload(lines, cursor, "full imp", df, &parsed, error));
        assert(parsed);
        assert(cursor == lines.size());
        assert(df.activeImputationVersion == 2);

        const DataColumn *score = FindDataColumnInDataFrame(df, "score");
        assert(score != nullptr);
        assert(score->imputedMissing.size() == 2);
        assert(score->imputedMissing[0]);
        assert(!score->imputedMissing[1]);
        assert(score->imputationOriginalValues[0] == "NA");
        assert(score->imputationValues.size() == 2);
        assert(score->imputationValues[0][0] == "10");
        assert(score->imputationValues[1][0] == "11");
        assert(score->values[0] == "11");
        assert(score->values[1] == "5");
    }

    {
        DataFrameModel df;
        df.group = "imp data";
        df.rows = 2;
        df.datasetType = "multiple_imputation";
        df.imputationId = "imp1";
        df.sourceDatasetId = "source1";
        df.imputationCount = 2;
        df.activeImputationVersion = 1;
        df.imputationDisplayMode = "version";
        DataColumn age;
        age.name = "age";
        age.type = "numeric";
        age.values = {"10", "20"};
        age.imputedMissing = {false, true};
        age.imputationOriginalSparse[1] = "NA";
        age.imputationValuesSparse = {
            {{1, "20"}},
            {{1, "21"}}
        };
        df.columns.push_back(age);

        std::ostringstream payload;
        WriteDataFramePayloadForR(payload, df, true);
        std::string text = payload.str();
        assert(text.find("DATASET\nimp data\n2\n1\nage\nnumeric\n10\n20\n") == 0);
        assert(text.find("IMPUTATION_SPARSE\nmultiple_imputation\nimp1\nsource1\n2\n1\nversion\n1\nage\n1\n2\nNA\n20\n21\n") != std::string::npos);
    }

    {
        std::vector<std::string> lines = {"DATAFRAME", "1"};
        std::size_t cursor = 0;
        DataFrameModel df;
        bool parsed = false;
        std::string error;
        assert(!ParseDataFramePayload(lines, cursor, "bad", df, &parsed, error));
        assert(parsed);
        assert(error == "ERR malformed dataframe payload");
    }

    return 0;
}
