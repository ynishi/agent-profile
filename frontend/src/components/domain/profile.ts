import { IProfile } from "../../interfaces";

export const result2Profile = (value: any): IProfile => {
  if (!value) {
    return {} as IProfile;
  }
  return {
    profileId: value["profile_id"],
    name: value["name"],
    description: value["description"],
    personalities: value["personalities"] ?? [],
    expertise: value["expertise"] ?? {},
    behavior: value["behavior"] ?? {},
    mission: value["mission"] ?? {},
    policies: value["policies"] ?? [],
  } as IProfile;
};

export const values2Request = (values: any): any => {
  const behavior = {
    stance: values["stance"],
    style: values["style"],
  };
  const mission = {
    statement: values["statement"],
    purpose: values["purpose"],
  };
  const expertise = {
    domain: values["domain"],
    level: values["level"],
  };
  const request = {
    ...values,
    behavior,
    mission,
    expertise,
  };
  delete request.stance;
  delete request.style;
  delete request.statement;
  delete request.purpose;
  delete request.domain;
  delete request.level;
  return request;
};
