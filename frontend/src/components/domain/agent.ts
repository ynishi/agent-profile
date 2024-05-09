import { IAgent } from "../../interfaces";

export const values2Request = (values: any): any => {
  const function_ids = values["function_ids"]?.map((r: any) => r.functionId);
  const request = {
    ...values,
    attributes: values["attributes"] ?? [],
    function_ids: function_ids ?? [],
  };

  return request;
};

export const result2Agent = (value: any): IAgent => {
  if (!value) {
    return {} as IAgent;
  }
  return {
    agentId: value["agent_id"],
    name: value["name"],
    description: value["description"],
    version: value["version"],
    profileId: value["profile_id"],
    flowId: value["flow_id"],
    functionIds: value["function_ids"],
    attributes: value["attributes"],
  };
};
