import { NumberField, Show, TextField } from "@refinedev/antd";
import { useSelect, useShow } from "@refinedev/core";
import { Typography } from "antd";

import { result2Flow } from "../../components/domain/flow";
import { Attributes } from "../../components/view/showAttributes";
import { IStep } from "../../interfaces";

const { Title } = Typography;

export const FlowShow = () => {
  const { queryResult } = useShow({});
  const { data, isLoading } = queryResult;

  const { options } = useSelect<IStep>({
    resource: "steps",
    optionLabel: ((item: any) => `${item.name} - ${item.step_id}`) as any,
    optionValue: "step_id" as any,
  });

  const optionMap = new Map(options?.map((x) => [x?.value, x?.label]));

  const record = data?.data;
  const flow = result2Flow(record);

  return (
    <Show isLoading={isLoading}>
      <Title level={5}>{"FlowId"}</Title>
      <TextField value={flow.flowId} />
      <Title level={5}>{"Name"}</Title>
      <TextField value={flow.name} />
      <Title level={5}>{"Description"}</Title>
      <TextField value={flow.description} />
      <Title level={5}>{"StepId"}</Title>
      {flow.stepIds?.map((sid) => (
        <p>
          <TextField value={optionMap.get(sid) as any} />
        </p>
      ))}
      <Title level={5}>{"Error Policy"}</Title>
      <TextField value={flow.errorPolicy} />
    </Show>
  );
};
