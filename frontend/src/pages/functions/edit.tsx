import { Edit, useForm } from "@refinedev/antd";
import { Form, Input } from "antd";
import { values2Request } from "../../components/domain/function";
import { Attributes } from "../../components/view/attributes";

export const FunctionEdit = () => {
  const { formProps, saveButtonProps, onFinish } = useForm({});
  const handleOnFinish = (values: any) => {
    onFinish(values2Request(values));
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
    </Edit>
  );
};
