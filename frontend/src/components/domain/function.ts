import { IFunction } from "../../interfaces";

export const result2Function = (value: any): IFunction => {
  if (!value) {
    return {} as IFunction;
  }
  return {
    functionId: value["function_id"],
    name: value["name"],
    description: value["description"],
    content: value["content"],
    inputData: value["input_data"],
    outputData: value["output_data"],
    attributes: value["attributes"],
  } as IFunction;
};

export const values2Request = (values: any): any => {
  const request = {
    ...values,
  };
  return request;
};
