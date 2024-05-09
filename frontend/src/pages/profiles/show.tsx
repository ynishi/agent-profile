import { Show, TextField } from "@refinedev/antd";
import { useShow } from "@refinedev/core";

import { result2Profile } from "../../components/domain/profile";
import { Policies } from "../../components/view/showPolicies";
import { Personalities } from "../../components/view/showPersonalities";
import { groupLevel, Title, Text } from "../../components/view/consts";

export const ProfileShow = () => {
  const { queryResult } = useShow({});
  const { data, isLoading } = queryResult;

  const record = data?.data;
  const profile = result2Profile(record);

  return (
    <Show isLoading={isLoading}>
      <Title level={5}>{"ProfileId"}</Title>
      <TextField value={profile.profileId} />
      <Title level={5}>{"Name"}</Title>
      <TextField value={profile.name} />
      <Title level={5}>{"Description"}</Title>
      <TextField value={profile.description} />
      <Personalities value={profile.personalities} />
      <Title level={groupLevel}>{"Mission"}</Title>
      <Title level={5}>{"Statement"}</Title>
      <TextField value={profile.mission?.statement} />
      <Title level={5}>{"Purpose"}</Title>
      <TextField value={profile.mission?.purpose} />
      <Title level={groupLevel}>{"Expertise"}</Title>
      <Title level={5}>{"Domain"}</Title>
      <TextField value={profile.expertise?.domain} />
      <Title level={5}>{"Level"}</Title>
      <TextField value={profile.expertise?.level} />
      <Title level={groupLevel}>{"Behavior"}</Title>
      <Title level={5}>{"Stance"}</Title>
      <TextField value={profile.behavior?.stance} />
      <Title level={5}>{"Style"}</Title>
      <TextField value={profile.behavior?.style} />
      <Policies value={profile.policies} />
    </Show>
  );
};
