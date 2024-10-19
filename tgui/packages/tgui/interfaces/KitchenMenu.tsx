import { useBackend } from '../backend';
import { Box, Button, Divider, Flex, Grid, Input, NoticeBox, NumberInput, Section, Table, Stack, Tabs } from '../components';
import { Window } from '../layouts';

const STATE_CUSTOMIZING_MENU = 'customizing_menu';
const STATE_VIEWING_MENU = 'viewing_menu';

export const KitchenMenu = (props, context) => {
  const { act, data } = useBackend(context);
  const { authorized, page } = data;
  return (
    <Window width={500} height={600} resizable>
      <Window.Content>{authorized ? <ChefKitchenMenuContent /> : <ChefKitchenMenuContent />}</Window.Content>
    </Window>
  );
};

export const ChefKitchenMenuContent = (_, context) => {
  const { act, data } = useBackend(context);
  const { authorized, page } = data;

  return (
    <Window title="Chef Kitchen Menu" width={500} height={600} resizable>
      <Window.Content>
        <Stack fill>
          <Stack.Item width="100%">
            <Section title="Menu Selection" fitted fluid>
              <Tabs fluid>
                <Tabs.Tab
                  fluid
                  icon="desktop"
                  selected={page === STATE_VIEWING_MENU}
                  onClick={() => act('setState', { state: STATE_VIEWING_MENU })}>
                  Menu
                </Tabs.Tab>
                <Tabs.Tab
                  fluid
                  icon="desktop"
                  selected={page === STATE_CUSTOMIZING_MENU}
                  onClick={() => act('setState', { state: STATE_CUSTOMIZING_MENU })}>
                  Menu Customization
                </Tabs.Tab>
              </Tabs>
            </Section>
          </Stack.Item>
          <Stack.Item grow position="relative">
            {(page === STATE_CUSTOMIZING_MENU && <PageMenuCustomization />) ||
              (page === STATE_VIEWING_MENU && <PageMenuView />)}
          </Stack.Item>
        </Stack>
      </Window.Content>
    </Window>
  );
};

const PageMenuCustomization = (props, context) => {
  const { act, data } = useBackend(context);
  const { authorized, page } = data;

  return <Window>Customization</Window>;
};

const PageMenuView = (props, context) => {
  const { act, data } = useBackend(context);
  const { authorized, page } = data;

  return <Window>View</Window>;
};
