import { IFlow, IStep } from "../../interfaces";

export const stepIdsCol = "step_ids";
export const stepId = "stepId";

export const values2Request = (values: any): any => {
  const step_ids = values[stepIdsCol].map((r: any) => r.stepId);

  const request = {
    ...values,
    step_ids,
  };
  delete request.stepIds;
  return request;
};

export const functionIdsCol = "function_ids";
export const functionId = "functionId";

export const values2RequestStep = (values: any): any => {
  const function_ids = values[functionIdsCol].map((r: any) => r.functionId);

  const request = {
    ...values,
    function_ids,
  };
  delete request.functionIds;
  return request;
};

export const result2Flow = (value: any): IFlow => {
  if (!value) {
    return {} as IFlow;
  }
  return {
    flowId: value["flow_id"],
    name: value["name"],
    description: value["description"],
    stepIds: value[stepIdsCol] ?? [],
    attributes: value["attributes"],
    errorPolicy: value["error_policy"],
  } as IFlow;
};

export const result2Step = (value: any): IStep => {
  if (!value) {
    return {} as IStep;
  }
  return {
    stepId: value["step_id"],
    name: value["name"],
    description: value["description"],
    content: value["content"],
    condition: value["condition"],
    functionIds: value[functionIdsCol] ?? [],
    attributes: value["attributes"],
    errorStepId: value["error_step_id"],
  } as IStep;
};
