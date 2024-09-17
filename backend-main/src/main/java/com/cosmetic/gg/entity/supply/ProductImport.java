package com.cosmetic.gg.entity.supply;

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "product_import")
@Entity
@Getter
@Setter
public class ProductImport extends EntityBase{

	@NotEmpty(message = "Id of product not empty")
	@NotNull(message = "Id of product can not null")
	@Column(name = "product_id")
	private String productId;
	
	@NotEmpty(message = "Id of import can not empty")
	@NotNull(message = "Id of import can not null")
	@Column(name = "import_id")
	private String importId;
}
