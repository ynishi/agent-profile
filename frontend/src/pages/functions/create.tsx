import { Create, useForm } from "@refinedev/antd";
import { Form, Input } from "antd";
import { values2Request } from "../../components/domain/function";
import { Attributes } from "../../components/view/attributes";
import { genUUID } from "../../components/utils";

export const FunctionCreate = () => {
  const { formProps, saveButtonProps, onFinish } = useForm({});

  const onFinishHandler = (values: any) => {
    onFinish(values2Request(values));
  };

  return (
    <Create saveButtonProps={saveButtonProps}>
      <Form {...formProps} onFinish={onFinishHandler} layout="vertical">
        <Form.Item
          label={"FunctionId"}
          name={["function_id"]}
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
        <Form.Item label={"Content"} name={["content"]}>
          <Input />
        </Form.Item>
        <Form.Item label={"InputData"} name={["input_data"]}>
          <Input />
        </Form.Item>
        <Form.Item label={"OutputData"} name={["output_data"]}>
          <Input />
        </Form.Item>
        <Attributes />
      </Form>
    </Create>
  );
};
