import { Edit, useForm, useSelect } from "@refinedev/antd";
import { Button, Form, Input, Select, Space } from "antd";
import { IFunction, IStep } from "../../interfaces";
import {
  functionIdsCol,
  functionId,
  result2Step,
  values2RequestStep,
} from "../../components/domain/flow";
import { Attributes } from "../../components/view/attributes";
import { MinusCircleOutlined, PlusOutlined } from "@ant-design/icons";
import { Title, groupLevel } from "../../components/view/consts";

export const StepEdit = () => {
  const { formProps, saveButtonProps, onFinish, queryResult } = useForm({});
  const { selectProps } = useSelect<IFunction>({
    resource: "functions",
    optionLabel: ((item: any) => `${item.name} - ${item.function_id}`) as any,
    optionValue: "function_id" as any,
  });
  const { selectProps: stepSelectProps } = useSelect<IStep>({
    resource: "steps",
    optionLabel: ((item: any) => `${item.name} - ${item.step_id}`) as any,
    optionValue: "step_id" as any,
  });
  const step = result2Step(queryResult?.data?.data);

  const handleOnFinish = (values: any) => {
    onFinish(values2RequestStep(values));
  };
  return (
    <Edit saveButtonProps={saveButtonProps}>
      <Form {...formProps} onFinish={handleOnFinish} layout="vertical">
        <Form.Item
          label={"Name"}
          name={["name"]}
          rules={[
            {
              required: true,
            },
          ]}
        >
          <Input />
        </Form.Item>
        <Form.Item label={"Description"} name={["description"]}>
          <Input />
        </Form.Item>
        <Form.Item
          label={"Content"}
          name={["content"]}
          rules={[
            {
              required: true,
            },
          ]}
        >
          <Input />
        </Form.Item>
        <Form.Item label={"Condition"} name={["condition"]}>
          <Input />
        </Form.Item>
        <Title level={groupLevel}>Functions</Title>
        <Form.List name={[functionIdsCol]}>
          {(fields, { add, remove }) => (
            <>
              {fields.map(({ key, name, ...restField }) => (
                <Space
                  key={key}
                  style={{ display: "flex", marginBottom: 8 }}
                  align="baseline"
                >
                  <Form.Item
                    {...restField}
                    name={[name, functionId]}
                    rules={[{ required: true, message: "Missing funtion" }]}
                    labelAlign="left"
                    initialValue={step?.functionIds?.[key] as any}
                  >
                    <Select
                      {...selectProps}
                      style={{ width: 800, marginBottom: 24 }}
                    />
                  </Form.Item>
                  <MinusCircleOutlined onClick={() => remove(name)} />
                </Space>
              ))}
              <Form.Item>
                <Button
                  type="dashed"
                  onClick={() => add()}
                  block
                  icon={<PlusOutlined />}
                >
                  Add function
                </Button>
              </Form.Item>
            </>
          )}
        </Form.List>

        <Attributes />
        <Form.Item label={"Error Step"} name={["error_step_id"]}>
          <Select
            {...stepSelectProps}
            style={{ width: 800, marginBottom: 24 }}
            defaultValue={step.errorStepId as any}
          />
        </Form.Item>
      </Form>
    </Edit>
  );
};
