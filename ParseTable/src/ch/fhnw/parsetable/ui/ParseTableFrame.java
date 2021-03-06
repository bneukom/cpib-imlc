package ch.fhnw.parsetable.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.List;

import javax.swing.GroupLayout;
import javax.swing.GroupLayout.Alignment;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
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
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;

import ch.fhnw.parsetable.BNFGrammar.NT;
import ch.fhnw.parsetable.BNFGrammar.Production;
import ch.fhnw.parsetable.BNFGrammar.Symbol;
import ch.fhnw.parsetable.BNFGrammar.T;
import ch.fhnw.parsetable.ParseTable;
import ch.fhnw.parsetable.ParseTableGenerator;

public class ParseTableFrame extends JFrame {

	private final JPanel contentPane;
	private final JTable parseTable;
	private final JTextArea parseTableTextArea;

	private JTextPane logTextPane;
	private JTextPane errorTextPane;
	private Style errorStyle;

	private ParseTable currentParseTable;
	private JScrollPane tableScrollPane;

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

		final JMenuItem mntmExport = new JMenuItem("Export");
		mntmExport.addActionListener(new ActionListener() {
			public void actionPerformed(final ActionEvent e) {
				final JFileChooser fc = new JFileChooser();
				final int returnVal = fc.showSaveDialog(ParseTableFrame.this);
				if (returnVal == JFileChooser.APPROVE_OPTION && currentParseTable != null) {
					String export = "";
					final List<Production>[][] table = currentParseTable.table2();

					final List<T> termminals = currentParseTable.terminals2();
					String row0 = "";
					for (T t : termminals) {
						row0 += (t.s() + ";");
					}
					export += (row0 + "\n");
					
					final List<NT> nonTerminals = currentParseTable.nonTerminals2();
					for (int y = 0; y < table.length; ++y) {
						String row = "" + nonTerminals.get(y).s() + ";";
						for (int x = 0; x < table[y].length; ++x) {
							row += (productionsString(table[y][x]) + ";");
						}
						export += (row + "\n");
					}
					final File selectedFile = fc.getSelectedFile();

					try {
						Files.write(Paths.get(selectedFile.toURI()), export.getBytes(), StandardOpenOption.CREATE);
					} catch (final IOException e1) {
						e1.printStackTrace();
					}
				}
			}
		});
		mnFile.add(mntmExport);
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

		tableScrollPane = new JScrollPane();
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
				logTextPane.setText("");
				errorTextPane.setText("");

				final ParseTableGenerator gen = new ParseTableGenerator();

				try {
					currentParseTable = gen.generateParseTable(parseTableTextArea.getText());
				} catch (final Exception exception) {
					System.err.println(exception.getMessage());
					parseTable.setModel(new DefaultTableModel());
					tableScrollPane.setRowHeader(null);
					return;
				}

				final ParseTableModel tableModel = new ParseTableModel(currentParseTable);

				parseTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
				parseTable.setModel(tableModel);

				final TableColumnModel rowHeaderColumnModel = new DefaultTableColumnModel() {
					boolean first = true;

					public void addColumn(final TableColumn tc) {
						if (first) {
							tc.setMaxWidth(175);
							super.addColumn(tc);
							first = false;
						}
					}
				};

				final RowHeaderModel rowHeaderModel = new RowHeaderModel(currentParseTable);

				final JTable headerColumn = new JTable(rowHeaderModel, rowHeaderColumnModel);
				headerColumn.createDefaultColumnsFromModel();
				headerColumn.getTableHeader().setReorderingAllowed(false);

				headerColumn.getColumnModel().getColumn(0).setHeaderValue("");
				parseTable.setSelectionModel(headerColumn.getSelectionModel());

				headerColumn.setBackground(Color.lightGray);
				headerColumn.setColumnSelectionAllowed(false);
				headerColumn.setCellSelectionEnabled(false);

				final JViewport viewport = new JViewport();
				viewport.setView(headerColumn);
				viewport.setPreferredSize(headerColumn.getMaximumSize());

				tableScrollPane.setRowHeader(viewport);
				tableScrollPane.setCorner(ScrollPaneConstants.UPPER_LEFT_CORNER, headerColumn.getTableHeader());

				final ParseTableCellRenderer cellRenderer = new ParseTableCellRenderer();
				final List<T> terminals = currentParseTable.terminals2();
				for (int columnIndex = 0; columnIndex < terminals.size(); ++columnIndex) {
					final T terminal = terminals.get(columnIndex);
					parseTable.getColumnModel().getColumn(columnIndex).setHeaderValue(terminal.s());
					parseTable.getColumnModel().getColumn(columnIndex).setCellRenderer(cellRenderer);
				}

				final int columnCount = tableModel.getColumnCount();
				final int rowCount = tableModel.getRowCount();
				outer: for (int c = 0; c < columnCount; c++) {
					for (int r = 0; r < rowCount; r++) {
						final Object valueAt = tableModel.getValueAt(r, c);
						if (valueAt != null) {
							@SuppressWarnings("unchecked")
							final List<Production> p = (List<Production>) valueAt;
							if (p.size() > 1) {
								parseTable.changeSelection(r, c, false, false);
								parseTable.scrollRectToVisible(parseTable.getCellRect(r, c, true));
								break outer;
							}
						}
					}
				}
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
		parseTableTextArea.setFont(new Font("Consolas", Font.PLAIN, 13));
		parseTableScrollPane.setViewportView(parseTableTextArea);
		panel_2.setLayout(gl_panel_2);

		final JScrollPane logScrollPane = new JScrollPane();

		logTextPane = new JTextPane();
		logTextPane.setFont(new Font("Consolas", Font.PLAIN, 13));
		logTextPane.setEditable(false);
		logScrollPane.setViewportView(logTextPane);

		final JPopupMenu popupMenu = new JPopupMenu();
		addPopup(logTextPane, popupMenu);

		final JMenuItem menuClearLog = new JMenuItem("Clear");
		menuClearLog.addActionListener(new ActionListener() {
			public void actionPerformed(final ActionEvent e) {
				logTextPane.setText("");
			}
		});
		popupMenu.add(menuClearLog);
		errorStyle = logTextPane.addStyle("errorStyle", null);

		final JScrollPane errorScrollPane = new JScrollPane();

		errorTextPane = new JTextPane();
		errorTextPane.setFont(new Font("Consolas", Font.PLAIN, 13));
		errorTextPane.setEditable(false);
		errorScrollPane.setViewportView(errorTextPane);

		final JPopupMenu errorPopupMenu = new JPopupMenu();
		addPopup(errorTextPane, errorPopupMenu);

		final JMenuItem menuClearErrors = new JMenuItem("Clear");
		errorPopupMenu.add(menuClearErrors);
		final GroupLayout gl_logPanel = new GroupLayout(logPanel);
		gl_logPanel.setHorizontalGroup(gl_logPanel.createParallelGroup(Alignment.LEADING).addGroup(
				gl_logPanel.createSequentialGroup().addComponent(logScrollPane, GroupLayout.DEFAULT_SIZE, 572, Short.MAX_VALUE).addGap(5)
						.addComponent(errorScrollPane, GroupLayout.DEFAULT_SIZE, 577, Short.MAX_VALUE).addGap(1)));
		gl_logPanel.setVerticalGroup(gl_logPanel.createParallelGroup(Alignment.LEADING).addComponent(logScrollPane, GroupLayout.PREFERRED_SIZE, 157, GroupLayout.PREFERRED_SIZE)
				.addComponent(errorScrollPane, GroupLayout.PREFERRED_SIZE, 157, GroupLayout.PREFERRED_SIZE));
		logPanel.setLayout(gl_logPanel);
		contentPane.setLayout(gl_contentPane);
		StyleConstants.setForeground(errorStyle, Color.red);
	}

	private static void packColumn(JTable table, int vColIndex, int margin) {
		DefaultTableColumnModel colModel = (DefaultTableColumnModel) table.getColumnModel();
		TableColumn col = colModel.getColumn(vColIndex);
		int width = 0;

		// Get width of column header
		TableCellRenderer renderer = col.getHeaderRenderer();
		if (renderer == null) {
			renderer = table.getTableHeader().getDefaultRenderer();
		}
		java.awt.Component comp = renderer.getTableCellRendererComponent(table, col.getHeaderValue(), false, false, 0, 0);
		width = comp.getPreferredSize().width;

		// Get maximum width of column data
		for (int r = 0; r < table.getRowCount(); r++) {
			renderer = table.getCellRenderer(r, vColIndex);
			comp = renderer.getTableCellRendererComponent(table, table.getValueAt(r, vColIndex), false, false, r, vColIndex);
			width = Math.max(width, comp.getPreferredSize().width);
		}

		// Add margin
		width += 2 * margin;

		// Set the width
		col.setPreferredWidth(width);
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

	private void appendToPane(final JTextPane pane, final String text, final Style style) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				final Document doc = pane.getDocument();
				try {
					doc.insertString(doc.getLength(), text, style);
				} catch (final BadLocationException e) {
					throw new RuntimeException(e);
				}

				pane.setCaretPosition(doc.getLength() - 1);
			}
		});
	}

	private void redirectSystemStreams() {
		final OutputStream out = new OutputStream() {
			@Override
			public void write(final int b) throws IOException {
				appendToPane(logTextPane, String.valueOf((char) b), null);
			}

			@Override
			public void write(final byte[] b, final int off, final int len) throws IOException {
				appendToPane(logTextPane, new String(b, off, len), null);
			}

			@Override
			public void write(final byte[] b) throws IOException {
				write(b, 0, b.length);
			}
		};

		final OutputStream err = new OutputStream() {
			@Override
			public void write(final int b) throws IOException {
				appendToPane(errorTextPane, String.valueOf((char) b), errorStyle);
			}

			@Override
			public void write(final byte[] b, final int off, final int len) throws IOException {
				appendToPane(errorTextPane, new String(b, off, len), errorStyle);
			}

			@Override
			public void write(final byte[] b) throws IOException {
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
		public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
			final List<Production> productions = (List<Production>) value;
			final String stringValue = value != null ? productionsString(productions) : "";
			final Component tableCellRendererComponent = super.getTableCellRendererComponent(table, stringValue, isSelected, hasFocus, row, column);

			// test
			if (productions != null && productions.size() > 1)
				tableCellRendererComponent.setForeground(Color.RED);
			else
				tableCellRendererComponent.setForeground(Color.BLACK);

			return tableCellRendererComponent;
		}

	}

	private static void addPopup(final Component component, final JPopupMenu popup) {
		component.addMouseListener(new MouseAdapter() {
			public void mousePressed(final MouseEvent e) {
				if (e.isPopupTrigger()) {
					showMenu(e);
				}
			}

			public void mouseReleased(final MouseEvent e) {
				if (e.isPopupTrigger()) {
					showMenu(e);
				}
			}

			private void showMenu(final MouseEvent e) {
				popup.show(e.getComponent(), e.getX(), e.getY());
			}
		});
	}
}
