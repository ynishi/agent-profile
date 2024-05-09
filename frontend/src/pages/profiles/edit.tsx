import { Edit, useForm } from "@refinedev/antd";
import { Form, Input } from "antd";
import {
  result2Profile,
  values2Request,
} from "../../components/domain/profile";
import { Policies } from "../../components/view/polilies";
import { Personalities } from "../../components/view/personalities";
import { Title, groupLevel } from "../../components/view/consts";

export const ProfileEdit = () => {
  const {
    formProps,
    saveButtonProps,
    onFinish,
    queryResult: profileResult,
  } = useForm({});
  const handleOnFinish = (values: any) => {
    onFinish(values2Request(values));
  };
  const profile = result2Profile(profileResult?.data?.data);

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
        <Personalities />
        <Title level={groupLevel}>{"Mission"}</Title>
        <Form.Item label={"Statement"} name={["statement"]}>
          <Input defaultValue={profile.mission?.statement} />
        </Form.Item>
        <Form.Item label={"Purpose"} name={["purpose"]}>
          <Input defaultValue={profile.mission?.purpose} />
        </Form.Item>
        <Title level={groupLevel}>{"Expertise"}</Title>
        <Form.Item label={"Domain"} name={["domain"]}>
          <Input defaultValue={profile.expertise?.domain} />
        </Form.Item>
        <Form.Item label={"Level"} name={["level"]}>
          <Input defaultValue={profile.expertise?.level} />
        </Form.Item>
        <Title level={groupLevel}>{"Behavior"}</Title>
        <Form.Item label={"Stance"} name={["stance"]}>
          <Input defaultValue={profile.behavior?.stance} />
        </Form.Item>
        <Form.Item label={"Style"} name={["style"]}>
          <Input defaultValue={profile.behavior?.style} />
        </Form.Item>
        <Policies />
      </Form>
    </Edit>
  );
};
