export interface IProfile {
  profileId: string;
  name: string;
  description: string?;
  personalities: IPersonality[]?;
  expertise: IExpertise?;
  behavior: IBehavior?;
  mission: IMission?;
  policies: IPolicy[]?;
}

export interface IPersonality {
  name: string;
  description: string?;
  content: string;
  weight: number;
}

export interface IMission {
  statement: string;
  purpose: string;
}

export interface IExpertise {
  domain: string;
  level: string;
}

export interface IBehavior {
  stance: string;
  style: string;
}

export interface IPolicy {
  name: string;
  description: string?;
  content: string;
  priority: number;
  attributes: any?;
}

export interface IAttributes {
  [key: string]: string;
}

export interface IFunction {
  functionId: string;
  name: string;
  description: string?;
  content: string;
  inputData: string?;
  outputData: string?;
  attributes: IAttributes?;
}

export interface IFlow {
  flowId: string;
  name: string;
  description: string?;
  stepIds: string[];
  attributes: IAttributes;
  errorPolicy: string?;
}

export interface IStep {
  stepId: string;
  name: string?;
  description: string?;
  content: string;
  condition: string?;
  functionIds: string[];
  attributes: IAttributes;
  errorStepId: string?;
}

export interface IAgent {
  agentId: string;
  name: string;
  description: string?;
  version: string;
  profileId: string;
  flowId: string;
  functionIds: string[];
  attributes: IAttributes;
}
