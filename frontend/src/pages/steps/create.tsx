import { Create, useForm, useSelect } from "@refinedev/antd";
import { Button, Form, Input, Select, Space } from "antd";
import {
  functionId,
  functionIdsCol,
  values2RequestStep,
} from "../../components/domain/flow";
import { genUUID } from "../../components/utils";
import { Attributes } from "../../components/view/attributes";
import { IFunction, IStep } from "../../interfaces";
import { Title, groupLevel } from "../../components/view/consts";
import { MinusCircleOutlined, PlusOutlined } from "@ant-design/icons";

export const StepCreate = () => {
  const { formProps, saveButtonProps, onFinish } = useForm({});
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

  const onFinishHandler = (values: any) => {
    onFinish(values2RequestStep(values));
  };

  return (
    <Create saveButtonProps={saveButtonProps}>
      <Form {...formProps} onFinish={onFinishHandler} layout="vertical">
        <Form.Item
          label={"stepId"}
          name={["step_id"]}
          hidden={true}
          initialValue={genUUID()}
        />
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
                    rules={[{ required: true, message: "Missing function" }]}
                    labelAlign="left"
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
          />
        </Form.Item>
      </Form>
    </Create>
  );
};
