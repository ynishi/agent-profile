import { Create, useForm } from "@refinedev/antd";
import { Form, Input } from "antd";
import { values2Request } from "../../components/domain/profile";
import { genUUID } from "../../components/utils";
import { Policies } from "../../components/view/polilies";
import { Personalities } from "../../components/view/personalities";
import Title from "antd/es/typography/Title";

export const ProfileCreate = () => {
  const { formProps, saveButtonProps, onFinish } = useForm({});

  const handleOnFinish = (values: any) => {
    onFinish(values2Request(values));
  };

  return (
    <Create saveButtonProps={saveButtonProps}>
      <Form {...formProps} onFinish={handleOnFinish} layout="vertical">
        <Form.Item
          label={"ProfileId"}
          name={["profile_id"]}
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
        <Personalities />
        <Title level={5}>{"Mission"}</Title>
        <Form.Item label={"Statement"} name={["statement"]}>
          <Input />
        </Form.Item>
        <Form.Item label={"Purpose"} name={["purpose"]}>
          <Input />
        </Form.Item>
        <Title level={5}>{"Expertise"}</Title>
        <Form.Item label={"Domain"} name={["domain"]}>
          <Input />
        </Form.Item>
        <Form.Item label={"Level"} name={["level"]}>
          <Input />
        </Form.Item>
        <Title level={5}>{"Behavior"}</Title>
        <Form.Item label={"Stance"} name={["stance"]}>
          <Input />
        </Form.Item>
        <Form.Item label={"Style"} name={["style"]}>
          <Input />
        </Form.Item>
        <Policies />
      </Form>
    </Create>
  );
};
