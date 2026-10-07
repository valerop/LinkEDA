#include "../../src/core/application_state.h"
#include <algorithm>
#include <cassert>
using namespace rlispstat::core;
int main() {
    ApplicationState app;
    DataFrameModel data; data.group = "typed-items"; data.rows = 4;
    DataColumn binary; binary.name = "binary"; binary.type = "numeric"; binary.values = {"0","1","0","1"};
    DataColumn ordinal; ordinal.name = "ordered"; ordinal.type = "numeric"; ordinal.values = {"1","2","3","2"};
    DataColumn numeric; numeric.name = "numeric"; numeric.type = "numeric"; numeric.values = {"1","3","2","4"};
    data.columns = {binary,ordinal,numeric}; assert(app.registerDataset(data));
    DimensionalityState state; state.id="factor";state.group=data.group;
    state.variables={"binary","ordered","numeric"}; app.dimensionalityModels()[state.id]=state;
    VariableTypeChangeEffects effects; std::string message;
    assert(app.setVariableType(data.group,"binary","factor",effects,&message));
    assert(app.dimensionalityModels().at(state.id).variables.size()==3);
    assert(app.setVariableType(data.group,"ordered","ordered",effects,&message));
    assert(app.dimensionalityModels().at(state.id).variables.size()==3);
    assert(app.dimensionalityModels().at(state.id).eligibleVariables.size()==3);
    assert(app.setVariableType(data.group,"ordered","factor",effects,&message));
    const auto &updated=app.dimensionalityModels().at(state.id);
    assert(updated.variables.size()==2);
    assert(std::find(updated.variables.begin(),updated.variables.end(),"binary")!=updated.variables.end());
}
