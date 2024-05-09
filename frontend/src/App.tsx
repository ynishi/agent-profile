import { Authenticated, GitHubBanner, Refine } from "@refinedev/core";
import { DevtoolsPanel, DevtoolsProvider } from "@refinedev/devtools";
import { RefineKbar, RefineKbarProvider } from "@refinedev/kbar";

import {
  AuthPage,
  ErrorComponent,
  ThemedLayoutV2,
  ThemedSiderV2,
  useNotificationProvider,
} from "@refinedev/antd";
import "@refinedev/antd/dist/reset.css";

import routerBindings, {
  CatchAllNavigate,
  DocumentTitleHandler,
  NavigateToResource,
  UnsavedChangesNotifier,
} from "@refinedev/react-router-v6";
import { dataProvider, liveProvider } from "@refinedev/supabase";
import { App as AntdApp } from "antd";
import { BrowserRouter, Outlet, Route, Routes } from "react-router-dom";
import authProvider from "./authProvider";
import { Header } from "./components/header";
import { ColorModeContextProvider } from "./contexts/color-mode";
import { AgentCreate, AgentEdit, AgentList, AgentShow } from "./pages/agents";
import {
  ProfileCreate,
  ProfileEdit,
  ProfileList,
  ProfileShow,
} from "./pages/profiles";
import {
  FunctionCreate,
  FunctionEdit,
  FunctionList,
  FunctionShow,
} from "./pages/functions";
import { StepCreate, StepEdit, StepList, StepShow } from "./pages/steps";
import { FlowCreate, FlowEdit, FlowList, FlowShow } from "./pages/flows";
import { supabaseClient } from "./utility";

function App() {
  return (
    <BrowserRouter>
      <GitHubBanner />
      <RefineKbarProvider>
        <ColorModeContextProvider>
          <AntdApp>
            <DevtoolsProvider>
              <Refine
                dataProvider={dataProvider(supabaseClient)}
                liveProvider={liveProvider(supabaseClient)}
                authProvider={authProvider}
                routerProvider={routerBindings}
                notificationProvider={useNotificationProvider}
                resources={[
                  {
                    name: "agents",
                    list: "/agents",
                    create: "/agents/create",
                    edit: "/agents/edit/:id",
                    show: "/agents/show/:id",
                    meta: {
                      idColumnName: "agent_id",
                      canDelete: true,
                    },
                  },
                  {
                    name: "profiles",
                    list: "/profiles",
                    create: "/profiles/create",
                    edit: "/profiles/edit/:id",
                    show: "/profiles/show/:id",
                    meta: {
                      idColumnName: "profile_id",
                      canDelete: true,
                    },
                  },
                  {
                    name: "functions",
                    list: "/functions",
                    create: "/functions/create",
                    edit: "/functions/edit/:id",
                    show: "/functions/show/:id",
                    meta: {
                      idColumnName: "function_id",
                      canDelete: true,
                    },
                  },
                  {
                    name: "flows",
                    list: "/flows",
                    create: "/flows/create",
                    edit: "/flows/edit/:id",
                    show: "/flows/show/:id",
                    meta: {
                      idColumnName: "flow_id",
                      canDelete: true,
                    },
                  },
                  {
                    name: "steps",
                    list: "/steps",
                    create: "/steps/create",
                    edit: "/steps/edit/:id",
                    show: "/steps/show/:id",
                    meta: {
                      idColumnName: "step_id",
                      canDelete: true,
                    },
                  },
                ]}
                options={{
                  syncWithLocation: true,
                  warnWhenUnsavedChanges: true,
                  useNewQueryKeys: true,
                  projectId: "5zkYfb-AlvdWZ-kKDvFZ",
                }}
              >
                <Routes>
                  <Route
                    element={
                      <Authenticated
                        key="authenticated-inner"
                        fallback={<CatchAllNavigate to="/login" />}
                      >
                        <ThemedLayoutV2
                          Header={() => <Header sticky />}
                          Sider={(props) => <ThemedSiderV2 {...props} fixed />}
                        >
                          <Outlet />
                        </ThemedLayoutV2>
                      </Authenticated>
                    }
                  >
                    <Route
                      index
                      element={<NavigateToResource resource="agents" />}
                    />
                    <Route path="/agents">
                      <Route index element={<AgentList />} />
                      <Route path="create" element={<AgentCreate />} />
                      <Route path="edit/:id" element={<AgentEdit />} />
                      <Route path="show/:id" element={<AgentShow />} />
                    </Route>
                    <Route path="/profiles">
                      <Route index element={<ProfileList />} />
                      <Route path="create" element={<ProfileCreate />} />
                      <Route path="edit/:id" element={<ProfileEdit />} />
                      <Route path="show/:id" element={<ProfileShow />} />
                    </Route>
                    <Route path="/flows">
                      <Route index element={<FlowList />} />
                      <Route path="create" element={<FlowCreate />} />
                      <Route path="edit/:id" element={<FlowEdit />} />
                      <Route path="show/:id" element={<FlowShow />} />
                    </Route>
                    <Route path="/steps">
                      <Route index element={<StepList />} />
                      <Route path="create" element={<StepCreate />} />
                      <Route path="edit/:id" element={<StepEdit />} />
                      <Route path="show/:id" element={<StepShow />} />
                    </Route>
                    <Route path="/functions">
                      <Route index element={<FunctionList />} />
                      <Route path="create" element={<FunctionCreate />} />
                      <Route path="edit/:id" element={<FunctionEdit />} />
                      <Route path="show/:id" element={<FunctionShow />} />
                    </Route>
                    <Route path="*" element={<ErrorComponent />} />
                  </Route>
                  <Route
                    element={
                      <Authenticated
                        key="authenticated-outer"
                        fallback={<Outlet />}
                      >
                        <NavigateToResource />
                      </Authenticated>
                    }
                  >
                    <Route
                      path="/login"
                      element={
                        <AuthPage
                          type="login"
                          formProps={{
                            initialValues: {
                              email: "info@refine.dev",
                              password: "refine-supabase",
                            },
                          }}
                        />
                      }
                    />
                    <Route
                      path="/register"
                      element={<AuthPage type="register" />}
                    />
                    <Route
                      path="/forgot-password"
                      element={<AuthPage type="forgotPassword" />}
                    />
                  </Route>
                </Routes>

                <RefineKbar />
                <UnsavedChangesNotifier />
                <DocumentTitleHandler />
              </Refine>
              <DevtoolsPanel />
            </DevtoolsProvider>
          </AntdApp>
        </ColorModeContextProvider>
      </RefineKbarProvider>
    </BrowserRouter>
  );
}

export default App;
