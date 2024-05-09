import { Show, TextField } from "@refinedev/antd";
import { useSelect, useShow } from "@refinedev/core";

import { result2Step } from "../../components/domain/flow";
import { Title } from "../../components/view/consts";
import { Attributes } from "../../components/view/showAttributes";
import { IFunction } from "../../interfaces";

export const StepShow = () => {
  const { queryResult } = useShow({});
  const { data, isLoading } = queryResult;
  const { options } = useSelect<IFunction>({
    resource: "functions",
    optionLabel: ((item: any) => `${item.name} - ${item.function_id}`) as any,
    optionValue: "function_id" as any,
  });

  const optionMap = new Map(options?.map((x) => [x?.value, x?.label]));

  const record = data?.data;
  const step = result2Step(record);

  return (
    <Show isLoading={isLoading}>
      <Title level={5}>{"stepId"}</Title>
      <TextField value={step.stepId} />
      <Title level={5}>{"Name"}</Title>
      <TextField value={step.name} />
      <Title level={5}>{"Description"}</Title>
      <TextField value={step.description} />
      <Title level={5}>{"Content"}</Title>
      <TextField value={step.content} />
      <Title level={5}>{"Condition"}</Title>
      <TextField value={step.condition} />
      <Title level={5}>{"FunctionIds"}</Title>
      {step.functionIds?.map((fid) => (
        <p>
          <TextField value={optionMap.get(fid) as any} />
        </p>
      ))}
      <Attributes value={step.attributes} />
      <Title level={5}>{"ErrorStepId"}</Title>
      <TextField value={step.errorStepId} />
    </Show>
  );
};
