package com.cosmetic.gg.entity.attribute;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Lob;
import javax.persistence.Table;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "value_detail")
@Entity
@Getter
@Setter
public class ValueDetail extends EntityBase{

	@Column(name = "import_price")
	private Float importPrice;
	
	@Column(name = "sell_price")
	private Float sellPrice;
	
	@Column(name = "import_quantity")
	private Integer importQuantity;
	
	@Column(name = "sell_quantity")
	private Integer sellQuantity;
	
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private EStatus status;
	
	@Column(name = "unit")
	private String unit;
	
	@Column(name = "description")
	private String description;
	
	@Column(name = "image")
	@Lob
    private byte[] image;
}
