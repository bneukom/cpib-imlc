package ch.fhnw.parsetable.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.List;

import javax.swing.GroupLayout;
import javax.swing.GroupLayout.Alignment;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.JViewport;
import javax.swing.LayoutStyle.ComponentPlacement;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;

import ch.fhnw.parsetable.BNFGrammar.Production;
import ch.fhnw.parsetable.BNFGrammar.Symbol;
import ch.fhnw.parsetable.BNFGrammar.T;
import ch.fhnw.parsetable.ParseTable;
import ch.fhnw.parsetable.ParseTableGenerator;
import javax.swing.JPopupMenu;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class ParseTableFrame extends JFrame {

	private final JPanel contentPane;
	private final JTable parseTable;
	private final JTextArea parseTableTextArea;
	private JTextPane logTextPane;
	private Style errorStyle;

	/**
	 * Launch the application.
	 */
	public static void main(final String[] args) {

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e1) {
			e1.printStackTrace();
		}

		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					final ParseTableFrame frame = new ParseTableFrame();
					frame.setVisible(true);
				} catch (final Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the frame.
	 */
	public ParseTableFrame() {
		setTitle("Parse Table Frame");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setBounds(100, 100, 1193, 734);
		redirectSystemStreams();

		final JMenuBar menuBar = new JMenuBar();
		setJMenuBar(menuBar);

		final JMenu mnFile = new JMenu("File");
		menuBar.add(mnFile);

		final JMenuItem mntmOpen = new JMenuItem("Open");
		mntmOpen.addActionListener(new ActionListener() {
			public void actionPerformed(final ActionEvent e) {

			}
		});
		mnFile.add(mntmOpen);
		contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		setContentPane(contentPane);

		final JPanel logPanel = new JPanel();
		logPanel.setBorder(new TitledBorder(null, "Log", TitledBorder.LEADING, TitledBorder.TOP, null, null));

		final JPanel inputParseTablePanel = new JPanel();
		inputParseTablePanel.setBorder(new TitledBorder(null, "Input/Parse Table", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		final GroupLayout gl_contentPane = new GroupLayout(contentPane);
		gl_contentPane.setHorizontalGroup(gl_contentPane.createParallelGroup(Alignment.LEADING).addComponent(inputParseTablePanel, GroupLayout.DEFAULT_SIZE, 1167, Short.MAX_VALUE)
				.addComponent(logPanel, GroupLayout.DEFAULT_SIZE, 1167, Short.MAX_VALUE));
		gl_contentPane.setVerticalGroup(gl_contentPane.createParallelGroup(Alignment.TRAILING).addGroup(
				gl_contentPane.createSequentialGroup().addComponent(inputParseTablePanel, GroupLayout.DEFAULT_SIZE, 468, Short.MAX_VALUE)
						.addPreferredGap(ComponentPlacement.RELATED).addComponent(logPanel, GroupLayout.PREFERRED_SIZE, 180, GroupLayout.PREFERRED_SIZE).addContainerGap()));
		inputParseTablePanel.setLayout(new BorderLayout(0, 0));

		final JSplitPane splitPane = new JSplitPane();
		inputParseTablePanel.add(splitPane, BorderLayout.CENTER);
		splitPane.setResizeWeight(0.5);
		splitPane.setDividerLocation(200);
		splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);

		final JScrollPane tableScrollPane = new JScrollPane();
		tableScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		tableScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
		splitPane.setRightComponent(tableScrollPane);

		parseTable = new JTable();
		tableScrollPane.setViewportView(parseTable);

		final JPanel panel_2 = new JPanel();
		splitPane.setLeftComponent(panel_2);

		final JScrollPane parseTableScrollPane = new JScrollPane();

		final JButton parseButton = new JButton("Generate Parse Table");
		parseButton.addActionListener(new ActionListener() {
			public void actionPerformed(final ActionEvent e) {
				final ParseTableGenerator gen = new ParseTableGenerator();
				final ParseTable generateParseTable = gen.generateParseTable(parseTableTextArea.getText());
				final ParseTableModel tableModel = new ParseTableModel(generateParseTable);

				parseTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
				parseTable.setModel(tableModel);

				final TableColumnModel rowHeaderColumnModel = new DefaultTableColumnModel() {
					boolean first = true;

					public void addColumn(TableColumn tc) {
						if (first) {
							tc.setMaxWidth(tc.getPreferredWidth());
							super.addColumn(tc);
							first = false;
						}
					}
				};

				final RowHeaderModel rowHeaderModel = new RowHeaderModel(generateParseTable);

				JTable headerColumn = new JTable(rowHeaderModel, rowHeaderColumnModel);
				headerColumn.createDefaultColumnsFromModel();
				headerColumn.getTableHeader().setReorderingAllowed(false);

				headerColumn.getColumnModel().getColumn(0).setHeaderValue("");
				parseTable.setSelectionModel(headerColumn.getSelectionModel());

				headerColumn.setBackground(Color.lightGray);
				headerColumn.setColumnSelectionAllowed(false);
				headerColumn.setCellSelectionEnabled(false);

				JViewport viewport = new JViewport();
				viewport.setView(headerColumn);
				viewport.setPreferredSize(headerColumn.getMaximumSize());

				tableScrollPane.setRowHeader(viewport);
				tableScrollPane.setCorner(ScrollPaneConstants.UPPER_LEFT_CORNER, headerColumn.getTableHeader());

				final ParseTableCellRenderer cellRenderer = new ParseTableCellRenderer();
				final List<T> terminals = generateParseTable.terminals2();
				for (int columnIndex = 0; columnIndex < terminals.size(); ++columnIndex) {
					final T terminal = terminals.get(columnIndex);
					parseTable.getColumnModel().getColumn(columnIndex).setHeaderValue(terminal.s());
					parseTable.getColumnModel().getColumn(columnIndex).setCellRenderer(cellRenderer);
				}

				int columnCount = tableModel.getColumnCount();
				int rowCount = tableModel.getRowCount();
				outer: for (int c = 0; c < columnCount; c++) {
					for (int r = 0; r < rowCount; r++) {
						Object valueAt = tableModel.getValueAt(r, c);
						if (valueAt != null) {
							@SuppressWarnings("unchecked")
							List<Production> p = (List<Production>) valueAt;
							if (p.size() > 1) {
								parseTable.changeSelection(r, c, false, false);
								parseTable.scrollRectToVisible(parseTable.getCellRect(r, c, true));
								break outer;
							}
						}
					}
				}

				System.out.println("\n============================\n\n");
			}
		});
		final GroupLayout gl_panel_2 = new GroupLayout(panel_2);
		gl_panel_2.setHorizontalGroup(gl_panel_2
				.createParallelGroup(Alignment.LEADING)
				.addComponent(parseTableScrollPane, GroupLayout.DEFAULT_SIZE, 1153, Short.MAX_VALUE)
				.addGroup(Alignment.TRAILING,
						gl_panel_2.createSequentialGroup().addContainerGap().addComponent(parseButton, GroupLayout.PREFERRED_SIZE, 199, GroupLayout.PREFERRED_SIZE).addGap(10)));
		gl_panel_2.setVerticalGroup(gl_panel_2.createParallelGroup(Alignment.LEADING).addGroup(
				Alignment.TRAILING,
				gl_panel_2.createSequentialGroup().addComponent(parseTableScrollPane, GroupLayout.DEFAULT_SIZE, 159, Short.MAX_VALUE).addPreferredGap(ComponentPlacement.RELATED)
						.addComponent(parseButton).addContainerGap()));

		parseTableTextArea = new JTextArea();
		parseTableScrollPane.setViewportView(parseTableTextArea);
		panel_2.setLayout(gl_panel_2);
		logPanel.setLayout(new BorderLayout(0, 0));

		final JScrollPane logScrollPane = new JScrollPane();
		logPanel.add(logScrollPane, BorderLayout.CENTER);

		logTextPane = new JTextPane();
		logTextPane.setEditable(false);
		logScrollPane.setViewportView(logTextPane);

		JPopupMenu popupMenu = new JPopupMenu();
		addPopup(logTextPane, popupMenu);

		JMenuItem mntmClear = new JMenuItem("Clear");
		mntmClear.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				logTextPane.setText("");
			}
		});
		popupMenu.add(mntmClear);
		contentPane.setLayout(gl_contentPane);
		errorStyle = logTextPane.addStyle("errorStyle", null);
		StyleConstants.setForeground(errorStyle, Color.red);
	}

	private void appendToPane(final String text, final Style style) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				Document doc = logTextPane.getDocument();
				try {
					doc.insertString(doc.getLength(), text, style);
				} catch (BadLocationException e) {
					throw new RuntimeException(e);
				}

				logTextPane.setCaretPosition(doc.getLength() - 1);
			}
		});
	}

	private void redirectSystemStreams() {
		OutputStream out = new OutputStream() {
			@Override
			public void write(int b) throws IOException {
				appendToPane(String.valueOf((char) b), null);
			}

			@Override
			public void write(byte[] b, int off, int len) throws IOException {
				appendToPane(new String(b, off, len), null);
			}

			@Override
			public void write(byte[] b) throws IOException {
				write(b, 0, b.length);
			}
		};

		OutputStream err = new OutputStream() {
			@Override
			public void write(int b) throws IOException {
				appendToPane(String.valueOf((char) b), errorStyle);
			}

			@Override
			public void write(byte[] b, int off, int len) throws IOException {
				appendToPane(new String(b, off, len), errorStyle);
			}

			@Override
			public void write(byte[] b) throws IOException {
				write(b, 0, b.length);
			}
		};

		System.setOut(new PrintStream(out, true));
		System.setErr(new PrintStream(err, true));
	}

	private static final class RowHeaderModel extends AbstractTableModel {

		private final ParseTable data;

		public RowHeaderModel(final ParseTable data) {
			super();
			this.data = data;
		}

		@Override
		public int getRowCount() {
			return data.nonTerminals2().size();
		}

		@Override
		public int getColumnCount() {
			return 1;
		}

		@Override
		public Object getValueAt(final int rowIndex, final int columnIndex) {
			return data.nonTerminals2().get(rowIndex).s();
		}

	}

	private static final class ParseTableModel extends AbstractTableModel {

		private final ParseTable data;
		private final List<Production>[][] tableData;

		public ParseTableModel(final ParseTable data) {
			super();
			this.data = data;
			this.tableData = this.data.table2();
		}

		@Override
		public int getRowCount() {
			return data.nonTerminals2().size();
		}

		@Override
		public int getColumnCount() {
			return data.terminals2().size();
		}

		@Override
		public Object getValueAt(final int rowIndex, final int columnIndex) {
			final List<Production> symbolsAt = tableData[rowIndex][columnIndex];
			if (symbolsAt != null && symbolsAt.size() > 0) {
				return symbolsAt;
			}
			return null;
		}

	}

	private static final class ParseTableCellRenderer extends DefaultTableCellRenderer {
		@Override
		@SuppressWarnings("unchecked")
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			final List<Production> productions = (List<Production>) value;
			final String stringValue = value != null ? productionsString(productions) : "";
			Component tableCellRendererComponent = super.getTableCellRendererComponent(table, stringValue, isSelected, hasFocus, row, column);

			// test
			if (productions != null && productions.size() > 1)
				tableCellRendererComponent.setForeground(Color.RED);
			else
				tableCellRendererComponent.setForeground(Color.BLACK);

			return tableCellRendererComponent;
		}

		private static String productionsString(final List<Production> productions) {
			String s = "";
			for (int i = 0; i < productions.size(); ++i) {
				final Production prod = productions.get(i);
				s += "[" + symbolString(prod.r2()) + (i < productions.size() - 1 ? "], " : "]");
			}
			return s;
		}

		private static String symbolString(final List<Symbol> input) {
			String s = "";
			for (int i = 0; i < input.size(); ++i) {
				final Symbol symbol = input.get(i);
				s += symbol.valueString() + (i < input.size() - 1 ? ", " : "");
			}
			return s;
		}
	}

	private static void addPopup(Component component, final JPopupMenu popup) {
		component.addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent e) {
				if (e.isPopupTrigger()) {
					showMenu(e);
				}
			}

			public void mouseReleased(MouseEvent e) {
				if (e.isPopupTrigger()) {
					showMenu(e);
				}
			}

			private void showMenu(MouseEvent e) {
				popup.show(e.getComponent(), e.getX(), e.getY());
			}
		});
	}
}
